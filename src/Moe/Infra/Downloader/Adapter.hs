-- | qBittorrent interpreter for Downloader effect.
module Moe.Infra.Downloader.Adapter
  ( -- * Interpreter
    runDownloaderQBittorrent,

    -- * Environment
    DownloaderEnv,
    initDownloaderEnv,

    -- * Re-exports
    module Moe.Infra.Downloader.Effect,
  )
where

import Control.Exception.Safe (tryAny)
import Data.Text qualified as T
import Data.Text.Read qualified as TR
import Effectful ((:>))
import Effectful.Dispatch.Dynamic (interpret)
import Moe.Domain.Setting (DownloaderConfig (..), UserPreference (..))
import Moe.Error (AppError (..))
import Moe.Infra.Downloader.Effect
import Moe.Infra.Setting.Effect (Setting, getSetting)
import Moe.Prelude
import Network.HTTP.Client (Manager)
import Network.QBittorrent.Client qualified as QB

-- | Opaque downloader environment caching an authenticated QBClient.
newtype DownloaderEnv = DownloaderEnv
  { cachedClient :: TVar (Maybe (QB.QBClient, DownloaderConfig))
  }

-- | Initialize downloader environment.
initDownloaderEnv :: (IOE :> es) => Eff es DownloaderEnv
initDownloaderEnv = do
  cachedClient <- newTVarIO Nothing
  pure DownloaderEnv {cachedClient}

-- | Classify qBittorrent error into structured error type.
classifyQBError :: QB.QBClientError -> DownloaderClientError
classifyQBError = \case
  QB.ServantError err -> DlNetworkError (show err)
  QB.QBApiError err
    | err.statusCode == 403 -> DlAuthError "IP is banned for too many failed login attempts"
    | otherwise -> DlApiError err.statusCode err.responseBody

-- | Run Downloader effect using qBittorrent, reading config from Setting.
--
-- Validates config, reuses a cached QBClient across job runs.
-- 'TestConnection' is handled independently of saved config since it
-- provides its own credentials.
runDownloaderQBittorrent ::
  (Setting :> es, Error AppError :> es, IOE :> es) =>
  DownloaderEnv ->
  Manager ->
  Eff (Downloader : es) a ->
  Eff es a
runDownloaderQBittorrent dlEnv manager =
  interpret $ \_ -> \case
    TestConnection url username password ->
      handleTestConnection manager url username password
    op -> do
      cfg <- (.downloader) <$> getSetting
      case validateConfig cfg of
        Just err -> handleUnconfigured err op
        Nothing -> do
          client <- getOrCreateClient dlEnv manager cfg
          handleDownloader cfg client op

-- | Handle operations when downloader is not configured.
-- Query operations return empty defaults; mutations still throw.
handleUnconfigured ::
  (Error AppError :> es) =>
  DownloaderClientError ->
  Downloader (Eff localEs) a ->
  Eff es a
handleUnconfigured err = \case
  GetRenameTorrents -> pure []
  GetTorrentsByHashes _ -> pure []
  GetTorrentFiles _ -> pure []
  _ -> throwError (DownloaderError err)

-- | Validate that all required fields in DownloaderConfig are non-empty.
validateConfig :: DownloaderConfig -> Maybe DownloaderClientError
validateConfig cfg =
  DlConfigError <$> fmtErrors "qBittorrent"
    ( validateField "url" cfg.url
        *> validateField "username" cfg.username
        *> validateField "password" cfg.password
        *> validateField "savePath" cfg.savePath
    )

-- | Return a cached client or create and authenticate a new one.
getOrCreateClient ::
  (Error AppError :> es, IOE :> es) =>
  DownloaderEnv ->
  Manager ->
  DownloaderConfig ->
  Eff es QB.QBClient
getOrCreateClient dlEnv manager cfg = do
  cached <- readTVarIO dlEnv.cachedClient
  case cached of
    Just (client, cachedCfg) | cachedCfg == cfg -> pure client
    _ -> do
      let qbConfig = toQBConfig cfg
          needsLogin = not (T.null cfg.username) && not (T.null cfg.password)
      client <- liftIO $ QB.initQBClientWith manager qbConfig
      when needsLogin $
        runQBAction client (void $ QB.login qbConfig)
      atomically $ writeTVar dlEnv.cachedClient (Just (client, cfg))
      pure client

-- | Test a qBittorrent connection using the provided credentials.
--
-- Creates a temporary client independent of any saved config.
handleTestConnection ::
  (IOE :> es) =>
  Manager ->
  Text ->
  Text ->
  Text ->
  Eff es (Either Text Text)
handleTestConnection manager url username password = do
  let qbConfig = mkQBConfig url username password
      toErr = Left . show . QB.clientErrorToQBClientError
  result <- liftIO $ tryAny $ do
    tmpClient <- QB.initQBClientWith manager qbConfig
    QB.runQB tmpClient (QB.login qbConfig) >>= \case
      Left e -> pure $ toErr e
      Right _ -> QB.runQB tmpClient QB.getVersion <&> first (show . QB.clientErrorToQBClientError)
  pure $ case result of
    Left ex -> Left (show ex)
    Right r -> r

-- | Handle a single Downloader operation using a pre-authenticated client.
handleDownloader ::
  (Error AppError :> es, IOE :> es) =>
  DownloaderConfig ->
  QB.QBClient ->
  Downloader (Eff localEs) a ->
  Eff es a
handleDownloader cfg client = \case
  TestConnection {} -> error "unreachable: handled in runDownloaderQBittorrent"
  AddTorrent params ->
    runQBAction client $ do
      let baseTags = [moeTag, renameTag]
          allTags = case params.tags of
            Nothing -> baseTags
            Just ts -> ordNub $ baseTags <> ts
          effectivePath = Just $ maybe cfg.savePath (\p -> cfg.savePath <> "/" <> p) params.savePath
          req =
            QB.AddTorrentRequest
              { urls = Just params.url,
                savepath = effectivePath,
                category = Nothing,
                tags = Just allTags,
                rename = params.rename,
                stopped = Just True
              }
      void $ QB.addTorrent req
  GetTorrentsByHashes hashes ->
    runQBAction client $ do
      let req =
            QB.TorrentInfoRequest
              { filter_ = Nothing,
                category = Nothing,
                tag = Nothing,
                hashes = if null hashes then Nothing else Just (map QB.InfoHash hashes)
              }
      QB.getTorrents (Just req)
  GetRenameTorrents ->
    runQBAction client $ do
      let req =
            QB.TorrentInfoRequest
              {
                filter_ = Nothing,
                category = Nothing,
                tag = Just renameTag,
                hashes = Nothing
              }
      QB.getTorrents (Just req)
  GetTorrentFiles hash ->
    runQBAction client $ QB.getTorrentContents (QB.InfoHash hash)
  RenameTorrentFile hash oldPath newPath ->
    runQBAction client $ void $ QB.renameFile (QB.InfoHash hash) oldPath newPath
  RenameTorrentFolder hash oldPath newPath ->
    runQBAction client $ void $ QB.renameFolder (QB.InfoHash hash) oldPath newPath
  SetTorrentLocation hashes location ->
    runQBAction client $ void $ QB.setLocation (map QB.InfoHash hashes) location
  StartTorrents hashes ->
    runQBAction client $ void $ QB.startTorrents (map QB.InfoHash hashes)
  StopTorrents hashes ->
    runQBAction client $ void $ QB.stopTorrents (map QB.InfoHash hashes)
  DeleteTorrents hashes deleteFiles ->
    runQBAction client $ void $ QB.deleteTorrents (map QB.InfoHash hashes) deleteFiles
  AddTagsToTorrents hashes tags ->
    runQBAction client $ void $ QB.addTags (map QB.InfoHash hashes) tags
  RemoveTagsFromTorrents hashes tags ->
    runQBAction client $ void $ QB.removeTags (map QB.InfoHash hashes) tags

-- | Run a single qBittorrent action on a pre-authenticated client.
runQBAction ::
  (Error AppError :> es, IOE :> es) =>
  QB.QBClient ->
  QB.ClientM a ->
  Eff es a
runQBAction client action = do
  result <- liftIO $ QB.runQB client action
  case result of
    Left e -> throwError $ DownloaderError $ classifyQBError (QB.clientErrorToQBClientError e)
    Right a -> pure a

-- | Convert DownloaderConfig to QBConfig.
toQBConfig :: DownloaderConfig -> QB.QBConfig
toQBConfig cfg = mkQBConfig cfg.url cfg.username cfg.password

-- | Build a QBConfig from URL, username, and password.
mkQBConfig :: Text -> Text -> Text -> QB.QBConfig
mkQBConfig url username password =
  let (host, port, useTLS) = parseUrl url
   in QB.QBConfig {host, port, credential = QB.Credential username password, useTLS}

-- | Parse URL to extract host, port, and TLS setting.
parseUrl :: Text -> (Text, Int, Bool)
parseUrl url =
  let stripped = stripProtocol url
      (host, portStr) = T.breakOn ":" stripped
      port = case TR.decimal (T.drop 1 portStr) of
        Right (p, _) -> p
        Left _ -> 8080
      useTLS = T.isPrefixOf "https://" url
   in (host, port, useTLS)
  where
    stripProtocol t
      | T.isPrefixOf "https://" t = T.drop 8 t
      | T.isPrefixOf "http://" t = T.drop 7 t
      | otherwise = t
