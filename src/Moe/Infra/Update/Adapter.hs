-- | GitHub interpreter for Update effect.
module Moe.Infra.Update.Adapter
  ( -- * Interpreter
    runUpdateGitHub,

    -- * Environment
    UpdateEnv,
    initUpdateEnv,
  )
where

import "crypton" Crypto.Hash (Digest, SHA256, hash)
import Control.Exception.Safe (tryAny)
import Data.Aeson qualified as Aeson
import Data.Aeson ((.:), (.:?), (.!=))
import Data.Text qualified as T
import Data.Time (NominalDiffTime, UTCTime, addUTCTime, getCurrentTime)
import Effectful ((:>))
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Log qualified as Log
import Moe.Error (AppError (..))
import Moe.Infra.Update.Effect
import Moe.Prelude
import Network.HTTP.Client
  ( Manager,
    Request (..),
    httpLbs,
    parseRequest,
    responseBody,
    responseStatus,
  )
import Network.HTTP.Types.Status (statusCode)
import System.Directory
  ( createDirectoryIfMissing,
    getPermissions,
    removePathForcibly,
    renameFile,
    setOwnerExecutable,
    setPermissions,
  )
import System.FilePath (takeDirectory, (</>))
import System.Process (callProcess)

-- -------------------------------------------------------------------
-- GitHub API response types
-- -------------------------------------------------------------------

data GitHubRelease = GitHubRelease
  { tagName :: Text,
    body :: Text,
    publishedAt :: Maybe UTCTime,
    assets :: [GitHubAsset]
  }
  deriving stock (Show)

instance Aeson.FromJSON GitHubRelease where
  parseJSON = Aeson.withObject "GitHubRelease" $ \o ->
    GitHubRelease
      <$> o .: "tag_name"
      <*> o .:? "body" .!= ""
      <*> o .:? "published_at"
      <*> o .: "assets"

data GitHubAsset = GitHubAsset
  { name :: Text,
    browserDownloadUrl :: Text
  }
  deriving stock (Show)

instance Aeson.FromJSON GitHubAsset where
  parseJSON = Aeson.withObject "GitHubAsset" $ \o ->
    GitHubAsset
      <$> o .: "name"
      <*> o .: "browser_download_url"

-- -------------------------------------------------------------------
-- Environment
-- -------------------------------------------------------------------

-- | Opaque update environment with version cache.
data UpdateEnv = UpdateEnv
  { currentVersion :: Text,
    cachedAbout :: TVar (Maybe CachedAbout)
  }

data CachedAbout = CachedAbout
  { aboutInfo :: AboutInfo,
    expiry :: UTCTime
  }

-- | Initialize update environment.
initUpdateEnv :: (IOE :> es) => Text -> Eff es UpdateEnv
initUpdateEnv currentVersion = do
  cachedAbout <- newTVarIO Nothing
  pure UpdateEnv {currentVersion, cachedAbout}

-- -------------------------------------------------------------------
-- Constants
-- -------------------------------------------------------------------

githubOwner :: Text
githubOwner = "birdgg"

githubRepo :: Text
githubRepo = "moe-bangumi"

cacheTTL :: NominalDiffTime
cacheTTL = 60

-- -------------------------------------------------------------------
-- Interpreter
-- -------------------------------------------------------------------

-- | Run Update effect using GitHub releases API.
runUpdateGitHub ::
  (Log :> es, Error AppError :> es, IOE :> es) =>
  UpdateEnv ->
  Manager ->
  Eff (Update : es) a ->
  Eff es a
runUpdateGitHub env manager =
  interpret $ \_ -> \case
    CheckForUpdate -> checkForUpdateImpl env manager
    PerformUpdate -> performUpdateImpl env manager

-- -------------------------------------------------------------------
-- CheckForUpdate implementation
-- -------------------------------------------------------------------

checkForUpdateImpl ::
  (Log :> es, Error AppError :> es, IOE :> es) =>
  UpdateEnv ->
  Manager ->
  Eff es AboutInfo
checkForUpdateImpl env manager = do
  now <- liftIO getCurrentTime
  mCached <- atomically $ do
    cached <- readTVar env.cachedAbout
    case cached of
      Just c | c.expiry > now -> pure (Just c.aboutInfo)
      _ -> pure Nothing
  case mCached of
    Just about -> pure about
    Nothing -> do
      about <- fetchAboutInfo env manager
      let newExpiry = addUTCTime cacheTTL now
      atomically $ writeTVar env.cachedAbout (Just CachedAbout {aboutInfo = about, expiry = newExpiry})
      pure about

-- | Fetch version info from GitHub releases API.
fetchAboutInfo ::
  (Log :> es, IOE :> es) =>
  UpdateEnv ->
  Manager ->
  Eff es AboutInfo
fetchAboutInfo env manager = do
  let currentVer = env.currentVersion
      emptyAbout =
        AboutInfo
          { currentVersion = currentVer,
            latestVersion = currentVer,
            needUpdate = False,
            autoUpdate = False,
            downloadUrl = "",
            checksumUrl = "",
            changelog = "",
            publishedAt = Nothing
          }

  platformInfo <- liftIO detectPlatform
  let tarballName = buildAssetName platformInfo

  result <- liftIO $ tryAny $ do
    let url = "https://api.github.com/repos/" <> toString githubOwner <> "/" <> toString githubRepo <> "/releases/latest"
    req <- parseRequest url
    let req' = req {requestHeaders = [("User-Agent", "moe-bangumi"), ("Accept", "application/vnd.github+json")]}
    resp <- httpLbs req' manager
    let status = statusCode (responseStatus resp)
    if status /= 200
      then pure emptyAbout
      else case Aeson.eitherDecode (responseBody resp) of
        Left _ -> pure emptyAbout
        Right (release :: GitHubRelease) -> do
          let latestVer = T.dropWhile (== 'v') release.tagName
              newer = isNewer latestVer currentVer
              canAuto = sameMajorMinor latestVer currentVer
              mTarball = find (\a -> a.name == tarballName) release.assets
              mChecksum = find (\a -> a.name == "checksums.txt") release.assets
          pure
            AboutInfo
              { currentVersion = currentVer,
                latestVersion = latestVer,
                needUpdate = newer,
                autoUpdate = newer && canAuto,
                downloadUrl = maybe "" (.browserDownloadUrl) mTarball,
                checksumUrl = maybe "" (.browserDownloadUrl) mChecksum,
                changelog = release.body,
                publishedAt = release.publishedAt
              }

  case result of
    Left err -> do
      Log.logAttention_ $ "Failed to check for update: " <> show err
      pure emptyAbout
    Right about -> pure about

-- -------------------------------------------------------------------
-- PerformUpdate implementation
-- -------------------------------------------------------------------

performUpdateImpl ::
  (Log :> es, Error AppError :> es, IOE :> es) =>
  UpdateEnv ->
  Manager ->
  Eff es ()
performUpdateImpl env manager = do
  about <- checkForUpdateImpl env manager
  if not about.needUpdate
    then pass
    else do
      Log.logInfo_ $ "Updating to version " <> about.latestVersion
      platformInfo <- liftIO detectPlatform
      let exeDir = takeDirectory platformInfo.executablePath
          tmpDir = exeDir </> ".update-tmp"
          tarballPath = tmpDir </> toString (buildAssetName platformInfo)
          extractedBinary = tmpDir </> "moe-bangumi"
          targetPath = platformInfo.executablePath

      -- Prepare temp directory
      liftIO $ do
        removePathForcibly tmpDir
        createDirectoryIfMissing True tmpDir

      -- Download tarball
      downloadFile about.downloadUrl tarballPath manager

      -- Download and verify checksum
      expectedHash <- downloadAndParseChecksum about.checksumUrl (buildAssetName platformInfo) manager
      actualHash <- liftIO $ sha256File tarballPath
      when (T.toLower expectedHash /= T.toLower actualHash) $
        throwError $ UpdateError $ UpdateChecksumMismatch expectedHash actualHash

      -- Extract binary from tarball
      extractResult <- liftIO $ tryAny $
        callProcess "tar" ["xzf", tarballPath, "-C", tmpDir, "--no-absolute-names"]
      case extractResult of
        Left err -> throwError $ UpdateError $ UpdateExtractionFailed $ "tar extraction failed: " <> show err
        Right () -> pass

      -- Set executable permission
      liftIO $ do
        perms <- getPermissions extractedBinary
        setPermissions extractedBinary (setOwnerExecutable True perms)

      -- Replace binary (renameFile may fail across filesystems)
      renameResult <- liftIO $ tryAny $ renameFile extractedBinary targetPath
      case renameResult of
        Left err -> throwError $ UpdateError $ UpdateFileError $ "Failed to replace binary: " <> show err
        Right () -> pass

      -- Cleanup
      liftIO $ removePathForcibly tmpDir

      Log.logInfo_ $ "Updated to " <> about.latestVersion <> ", exiting for restart"
      liftIO exitSuccess

-- -------------------------------------------------------------------
-- Helpers
-- -------------------------------------------------------------------

-- | Build tarball asset name for platform.
buildAssetName :: PlatformInfo -> Text
buildAssetName p = "moe-bangumi-" <> toText p.platform <> "-" <> toText p.arch <> ".tar.gz"

-- | Download a file from URL to local path.
downloadFile ::
  (Error AppError :> es, IOE :> es) =>
  Text ->
  FilePath ->
  Manager ->
  Eff es ()
downloadFile url dest manager = do
  result <- liftIO $ tryAny $ do
    req <- parseRequest (toString url)
    let req' = req {requestHeaders = [("User-Agent", "moe-bangumi")]}
    resp <- httpLbs req' manager
    let status = statusCode (responseStatus resp)
    when (status /= 200) $
      fail $ "HTTP " <> show status <> " downloading " <> toString url
    writeFileLBS dest (responseBody resp)
  case result of
    Left err -> throwError $ UpdateError $ UpdateNetworkError $ "Download failed: " <> show err
    Right () -> pass

-- | Download checksums.txt and extract hash for the target asset.
downloadAndParseChecksum ::
  (Error AppError :> es, IOE :> es) =>
  Text ->
  Text ->
  Manager ->
  Eff es Text
downloadAndParseChecksum url assetName manager = do
  result <- liftIO $ tryAny $ do
    req <- parseRequest (toString url)
    let req' = req {requestHeaders = [("User-Agent", "moe-bangumi")]}
    resp <- httpLbs req' manager
    let status = statusCode (responseStatus resp)
    when (status /= 200) $
      fail $ "HTTP " <> show status <> " downloading checksums"
    pure $ decodeUtf8 (responseBody resp)
  case result of
    Left err -> throwError $ UpdateError $ UpdateNetworkError $ "Checksum download failed: " <> show err
    Right content -> do
      let mHash = findChecksumForAsset assetName content
      case mHash of
        Nothing -> throwError $ UpdateError $ UpdateNetworkError $ "Checksum not found for " <> assetName
        Just h -> pure h

-- | Parse checksums.txt to find hash for a specific file.
-- Format: "<sha256hash>  <filename>"
findChecksumForAsset :: Text -> Text -> Maybe Text
findChecksumForAsset targetName content =
  let entries = mapMaybe parseChecksumLine (lines content)
   in snd <$> find (\(n, _) -> n == targetName) entries
  where
    parseChecksumLine :: Text -> Maybe (Text, Text)
    parseChecksumLine line = case words line of
      [hashVal, fileName] -> Just (fileName, hashVal)
      _ -> Nothing

-- | Compute SHA256 hash of a file.
sha256File :: FilePath -> IO Text
sha256File path = do
  content <- readFileBS path
  let digest = hash content :: Digest SHA256
  pure $ show digest
