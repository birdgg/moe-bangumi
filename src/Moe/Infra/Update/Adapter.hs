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
import Control.Exception.Safe (finally, tryAny)
import Data.Aeson qualified as Aeson
import Data.Aeson ((.:), (.:?), (.!=))
import Data.Text qualified as T
import Data.Time (NominalDiffTime, UTCTime, addUTCTime)
import Effectful.Exception qualified as EE
import Effectful.Log qualified as Log
import Moe.Infra.Http.Effect (Http, getHttpManager)
import Moe.Infra.Update.Effect
import Moe.Prelude
import Network.HTTP.Client
  ( Manager,
    Request (..),
    brRead,
    httpLbs,
    parseRequest,
    responseBody,
    responseStatus,
    withResponse,
  )
import Network.HTTP.Types.Status (statusCode)
import System.Directory qualified as Dir
import System.FilePath ((</>))
import Data.ByteString qualified as BS
import System.IO (hClose, openBinaryFile)
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
    dataFolder :: FilePath,
    cachedAbout :: TVar (Maybe CachedAbout)
  }

data CachedAbout = CachedAbout
  { aboutInfo :: AboutInfo,
    expiry :: UTCTime
  }

-- | Initialize update environment.
initUpdateEnv :: (IOE :> es) => Text -> FilePath -> Eff es UpdateEnv
initUpdateEnv currentVersion dataFolder = do
  cachedAbout <- newTVarIO Nothing
  pure UpdateEnv {currentVersion, dataFolder, cachedAbout}

-- -------------------------------------------------------------------
-- Constants
-- -------------------------------------------------------------------

githubOwner :: Text
githubOwner = "birdgg"

githubRepo :: Text
githubRepo = "moe-bangumi"

cacheTTL :: NominalDiffTime
cacheTTL = 1200

-- -------------------------------------------------------------------
-- Interpreter
-- -------------------------------------------------------------------

-- | Run Update effect using GitHub releases API.
runUpdateGitHub ::
  (Http :> es, Log :> es, Error UpdateError :> es, FileSystem :> es, Environment :> es, Time :> es, IOE :> es) =>
  UpdateEnv ->
  Eff (Update : es) a ->
  Eff es a
runUpdateGitHub env =
  interpret $ \_ -> \case
    CheckForUpdate -> do
      mgr <- getHttpManager
      checkForUpdateImpl env mgr
    PerformUpdate -> do
      mgr <- getHttpManager
      performUpdateImpl env mgr

-- -------------------------------------------------------------------
-- CheckForUpdate implementation
-- -------------------------------------------------------------------

checkForUpdateImpl ::
  (Log :> es, Error UpdateError :> es, Environment :> es, Time :> es, IOE :> es) =>
  UpdateEnv ->
  Manager ->
  Eff es AboutInfo
checkForUpdateImpl env manager = do
  now <- currentTime
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
  (Log :> es, Environment :> es, IOE :> es) =>
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

  platformInfo <- detectPlatform
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
              mTarball = find (\a -> a.name == tarballName) release.assets
              mChecksum = find (\a -> a.name == "checksums.txt") release.assets
              hasAssets = isJust mTarball && isJust mChecksum
          pure
            AboutInfo
              { currentVersion = currentVer,
                latestVersion = latestVer,
                needUpdate = newer,
                autoUpdate = newer && hasAssets,
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
  (Log :> es, Error UpdateError :> es, FileSystem :> es, Environment :> es, Time :> es, IOE :> es) =>
  UpdateEnv ->
  Manager ->
  Eff es ()
performUpdateImpl env manager = do
  about <- checkForUpdateImpl env manager
  if not about.needUpdate
    then pass
    else
      if T.null about.downloadUrl || T.null about.checksumUrl
        then Log.logAttention_ "Update available but no download assets found for this platform"
        else do
      Log.logInfo_ $ "Updating to version " <> about.latestVersion
      platformInfo <- detectPlatform
      let tmpDir = env.dataFolder </> ".update-tmp"
          tarballPath = tmpDir </> toString (buildAssetName platformInfo)
          extractedBinary = tmpDir </> "moe-bangumi"
          targetPath = platformInfo.executablePath

      -- Prepare temp directory
      removePathForcibly tmpDir
      createDirectoryIfMissing True tmpDir

      -- Download tarball
      downloadFile about.downloadUrl tarballPath manager

      -- Download and verify checksum
      expectedHash <- downloadAndParseChecksum about.checksumUrl (buildAssetName platformInfo) manager
      actualHash <- liftIO $ sha256File tarballPath
      when (T.toLower expectedHash /= T.toLower actualHash) $
        throwError $ UpdateChecksumMismatch expectedHash actualHash

      -- Extract binary from tarball
      extractResult <- liftIO $ tryAny $
        callProcess "tar" ["xzf", tarballPath, "-C", tmpDir]
      case extractResult of
        Left err -> throwError $ UpdateExtractionFailed $ "tar extraction failed: " <> show err
        Right () -> pass

      -- Set executable permission
      perms <- getPermissions extractedBinary
      setPermissions extractedBinary (Dir.setOwnerExecutable True perms)

      -- Replace binary using safe replacement with backup in writable tmpDir
      safeReplaceFile tmpDir extractedBinary targetPath

      -- Cleanup
      removePathForcibly tmpDir

      Log.logInfo_ $ "Updated to " <> about.latestVersion <> ", exiting for restart"
      liftIO exitSuccess

-- -------------------------------------------------------------------
-- Helpers
-- -------------------------------------------------------------------

-- | Build tarball asset name for platform.
buildAssetName :: PlatformInfo -> Text
buildAssetName p = "moe-bangumi-" <> toText p.platform <> "-" <> toText p.arch <> ".tar.gz"

-- | Download a file from URL to local path using streaming to avoid memory spikes.
downloadFile ::
  (Error UpdateError :> es, IOE :> es) =>
  Text ->
  FilePath ->
  Manager ->
  Eff es ()
downloadFile url dest manager = do
  result <- liftIO $ tryAny $ do
    req <- parseRequest (toString url)
    let req' = req {requestHeaders = [("User-Agent", "moe-bangumi")]}
    withResponse req' manager $ \resp -> do
      let status = statusCode (responseStatus resp)
      when (status /= 200) $
        fail $ "HTTP " <> show status <> " downloading " <> toString url
      h <- openBinaryFile dest WriteMode
      let loop = do
            chunk <- brRead (responseBody resp)
            unless (BS.null chunk) $ do
              BS.hPut h chunk
              loop
      loop `finally` hClose h
  case result of
    Left err -> throwError $ UpdateNetworkError $ "Download failed: " <> show err
    Right () -> pass

-- | Download checksums.txt and extract hash for the target asset.
downloadAndParseChecksum ::
  (Error UpdateError :> es, IOE :> es) =>
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
    Left err -> throwError $ UpdateNetworkError $ "Checksum download failed: " <> show err
    Right content -> do
      let mHash = findChecksumForAsset assetName content
      case mHash of
        Nothing -> throwError $ UpdateNetworkError $ "Checksum not found for " <> assetName
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

-- | Safe file replacement with backup in a writable temp directory.
--
-- Stores the backup in tmpDir (e.g. dataFolder) to avoid creating files
-- in potentially read-only directories like /app/ in Docker containers.
--
-- Steps:
-- 1. Backup: copy target → tmpDir/backup
-- 2. Replace: rename or copy source → target
-- 3. Cleanup: remove backup on success
--
-- On failure, automatically rollback using backup.
safeReplaceFile ::
  (IOE :> es, FileSystem :> es, Error UpdateError :> es, Log :> es) =>
  FilePath -> -- ^ Writable temp directory for backup
  FilePath -> -- ^ Source file (new binary)
  FilePath -> -- ^ Target file (current binary)
  Eff es ()
safeReplaceFile tmpDir source target = do
  let backupPath = tmpDir </> "moe-bangumi.backup"

  -- Backup current binary to writable temp directory
  backupResult <- EE.try @SomeException $ copyFile target backupPath

  -- Replace binary: try rename first (atomic, same filesystem),
  -- fall back to direct overwrite (cross-filesystem or restricted directory)
  result <- EE.try @SomeException $ do
    renameResult <- EE.try @SomeException $ renameFile source target
    case renameResult of
      Right () -> pass
      Left _ -> copyFileDirect source target

  case result of
    Right () -> do
      -- Clean up source and backup only after successful replacement
      whenM (doesFileExist source) $ removeFile source
      whenM (doesFileExist backupPath) $ removeFile backupPath
    Left err -> do
      Log.logAttention_ $ "Binary replacement failed, attempting rollback: " <> show err
      case backupResult of
        Right () -> do
          rollbackResult <- EE.try @SomeException $ copyFileDirect backupPath target
          case rollbackResult of
            Right () ->
              throwError $ UpdateFileError $
                "Failed to replace binary (rolled back): " <> show err
            Left rollbackErr ->
              throwError $ UpdateFileError $
                "Failed to replace binary and rollback failed: " <> show err <> ", rollback: " <> show rollbackErr
        Left _ ->
          throwError $ UpdateFileError $
            "Failed to replace binary (no backup available): " <> show err

-- | Copy file by writing to a temp file, unlinking the target, then renaming.
-- On Linux, a running executable cannot be opened for writing (ETXTBSY),
-- but it can be unlinked. The old inode stays alive for the running process,
-- and a new file is created at the same path.
copyFileDirect :: (IOE :> es, FileSystem :> es) => FilePath -> FilePath -> Eff es ()
copyFileDirect src dst = do
  content <- readFileBS src
  let tmpDst = dst <> ".new"
  writeFileBS tmpDst content
  perms <- getPermissions tmpDst
  setPermissions tmpDst (Dir.setOwnerExecutable True perms)
  removeFile dst
  renameFile tmpDst dst
