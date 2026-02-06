-- | qBittorrent interpreter for Download effect.
module Moe.Infrastructure.Download.Adapter
  ( -- * Interpreter
    runDownloadQBittorrent,

    -- * Environment
    DownloadEnv,
    initDownloadEnv,

    -- * Re-exports
    module Moe.Infrastructure.Download.Effect,
  )
where

import Data.Text qualified as T
import Data.Text.Read qualified as TR
import Effectful
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Error.Static (Error, throwError)
import Network.QBittorrent.Client qualified as QB
import Moe.Domain.Setting.Types (DownloaderConfig (..))
import Moe.Error (MoeError (..))
import Moe.Infrastructure.Download.Effect
import Moe.Prelude
import Network.HTTP.Client (Manager)
import System.FilePath (takeDirectory, (</>))

-- | Opaque download environment caching an authenticated QBClient.
newtype DownloadEnv = DownloadEnv
  { cachedClient :: TVar (Maybe (QB.QBClient, DownloaderConfig))
  }

-- | Initialize download environment.
initDownloadEnv :: (IOE :> es) => Eff es DownloadEnv
initDownloadEnv = do
  cachedClient <- newTVarIO Nothing
  pure DownloadEnv {cachedClient}

-- | Classify qBittorrent error into MoeError.
classifyQBError :: QB.QBError -> MoeError
classifyQBError = \case
  QB.NetworkError msg -> DownloaderError $ "Connection failed: " <> msg
  QB.AuthError msg -> DownloaderError $ "Authentication failed: " <> msg
  QB.ApiError code msg -> DownloaderError $ "API error " <> show code <> ": " <> msg
  QB.ParseError msg -> DownloaderError $ "Parse error: " <> msg
  QB.InvalidTorrent msg -> DownloaderError $ "Invalid torrent: " <> msg

-- | Run Download effect using qBittorrent.
--
-- Validates config, reuses a cached QBClient across job runs.
runDownloadQBittorrent ::
  (Error MoeError :> es, IOE :> es) =>
  DownloadEnv ->
  Manager ->
  DownloaderConfig ->
  Eff (Download : es) a ->
  Eff es a
runDownloadQBittorrent dlEnv manager cfg action =
  case validateConfig cfg of
    Just err -> throwError err
    Nothing -> do
      client <- getOrCreateClient dlEnv manager cfg
      interpret (\_ -> handleDownload cfg client) (inject action)

-- | Validate that all required fields in DownloaderConfig are non-empty.
validateConfig :: DownloaderConfig -> Maybe MoeError
validateConfig cfg =
  let emptyFields =
        [ name
          | (name, value) <-
              [ ("url", cfg.url),
                ("username", cfg.username),
                ("password", cfg.password),
                ("savePath", cfg.savePath)
              ],
            T.null value
        ]
   in if null emptyFields
        then Nothing
        else Just $ DownloaderError $
          "Configuration incomplete: " <> T.intercalate ", " emptyFields <> " must not be empty"

-- | Return a cached client or create and authenticate a new one.
getOrCreateClient ::
  (Error MoeError :> es, IOE :> es) =>
  DownloadEnv ->
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

-- | Handle a single Download operation using a pre-authenticated client.
handleDownload ::
  (Error MoeError :> es, IOE :> es) =>
  DownloaderConfig ->
  QB.QBClient ->
  Download (Eff localEs) a ->
  Eff es a
handleDownload cfg client = \case
  AddTorrent params ->
    runQBAction client $ do
      let baseTags = [moeTag, renameTag]
          allTags = case params.tags of
            Nothing -> baseTags
            Just ts -> ordNub $ baseTags <> ts
          tmpBase = takeDirectory (toString cfg.savePath) </> "tmp"
          effectivePath = Just $ toText $ maybe tmpBase ((tmpBase </>) . toString) params.savePath
          req =
            QB.AddTorrentRequest
              { urls = Just params.url,
                savepath = effectivePath,
                category = Nothing,
                tags = Just allTags,
                rename = params.rename,
                stopped = Nothing
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
              { filter_ = Nothing,
                category = Nothing,
                tag = Just renameTag,
                hashes = Nothing
              }
      QB.getTorrents (Just req)
  GetTorrentFiles hash ->
    runQBAction client $ QB.getTorrentFiles (QB.InfoHash hash)
  RenameTorrentFile hash oldPath newPath ->
    runQBAction client $ void $ QB.renameFile (QB.InfoHash hash) oldPath newPath
  SetTorrentLocation hashes location ->
    runQBAction client $ void $ QB.setLocation (map QB.InfoHash hashes) location
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
  (Error MoeError :> es, IOE :> es) =>
  QB.QBClient ->
  QB.ClientM a ->
  Eff es a
runQBAction client action = do
  result <- liftIO $ QB.runQB client action
  case result of
    Left e -> throwError $ classifyQBError (QB.clientErrorToQBError e)
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
