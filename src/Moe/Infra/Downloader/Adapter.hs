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
classifyQBError :: QB.QBError -> DownloaderClientError
classifyQBError = \case
  QB.NetworkError msg -> DlNetworkError msg
  QB.AuthError msg -> DlAuthError msg
  QB.ApiError code msg -> DlApiError code msg
  QB.ParseError msg -> DlParseError msg
  QB.InvalidTorrent msg -> DlInvalidTorrent msg

-- | Run Downloader effect using qBittorrent, reading config from Setting.
--
-- Validates config, reuses a cached QBClient across job runs.
runDownloaderQBittorrent ::
  (Setting :> es, Error AppError :> es, IOE :> es) =>
  DownloaderEnv ->
  Manager ->
  Eff (Downloader : es) a ->
  Eff es a
runDownloaderQBittorrent dlEnv manager =
  interpret $ \_ op -> do
    cfg <- (.downloader) <$> getSetting
    case validateConfig cfg of
      Just err -> throwError (DownloaderError err)
      Nothing -> do
        client <- getOrCreateClient dlEnv manager cfg
        handleDownloader manager cfg client op

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

-- | Handle a single Downloader operation using a pre-authenticated client.
handleDownloader ::
  (Error AppError :> es, IOE :> es) =>
  Manager ->
  DownloaderConfig ->
  QB.QBClient ->
  Downloader (Eff localEs) a ->
  Eff es a
handleDownloader manager cfg client = \case
  TestConnection url username password -> do
    let (host, port, useTLS) = parseUrl url
        qbConfig =
          QB.QBConfig
            { host = host,
              port = port,
              credential = QB.Credential username password,
              useTLS = useTLS
            }
        toErr e = Left $ show (QB.clientErrorToQBError e)
    result <- liftIO $ tryAny $ do
      tmpClient <- QB.initQBClientWith manager qbConfig
      QB.runQB tmpClient (QB.login qbConfig) >>= \case
        Left e -> pure $ toErr e
        Right _ -> QB.runQB tmpClient QB.getVersion <&> first (show . QB.clientErrorToQBError)
    pure $ case result of
      Left ex -> Left (show ex)
      Right r -> r
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
    runQBAction client $ QB.getTorrentFiles (QB.InfoHash hash)
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
    Left e -> throwError $ DownloaderError $ classifyQBError (QB.clientErrorToQBError e)
    Right a -> pure a

-- | Convert DownloaderConfig to QBConfig.
toQBConfig :: DownloaderConfig -> QB.QBConfig
toQBConfig cfg =
  let (host, port, useTLS) = parseUrl cfg.url
   in QB.QBConfig
        { host = host,
          port = port,
          credential = QB.Credential cfg.username cfg.password,
          useTLS = useTLS
        }

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
