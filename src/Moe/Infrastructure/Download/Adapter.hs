-- | qBittorrent interpreter for Download effect.
module Moe.Infrastructure.Download.Adapter
  ( -- * Interpreter
    runDownloadQBittorrent,

    -- * Re-exports
    module Moe.Infrastructure.Download.Effect,
    module Moe.Infrastructure.Download.Types,
  )
where

import Data.Text qualified as T
import Data.Text.Read qualified as TR
import Effectful
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Error.Static (Error, runErrorNoCallStack, throwError)
import Effectful.QBittorrent qualified as QB
import Moe.Domain.Setting.Types (DownloaderConfig (..))
import Moe.Error (MoeError (..))
import Moe.Infrastructure.Download.Effect
import Moe.Infrastructure.Download.Types
import Moe.Prelude
import System.FilePath (takeDirectory, (</>))

-- | Run Download effect using qBittorrent.
--
-- Takes downloader config and handles qBittorrent internals,
-- converting any QBError to MoeError.
runDownloadQBittorrent ::
  (Error MoeError :> es, IOE :> es) =>
  DownloaderConfig ->
  Eff (Download : es) a ->
  Eff es a
runDownloadQBittorrent cfg = interpret $ \_ -> \case
  AddTorrent params ->
    runQB cfg $ do
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
    runQB cfg $ do
      let req =
            QB.TorrentInfoRequest
              { filter_ = Nothing,
                category = Nothing,
                tag = Nothing,
                hashes = if null hashes then Nothing else Just (map QB.InfoHash hashes)
              }
      QB.getTorrents (Just req)

  GetTorrentsWithTag tag ->
    runQB cfg $ do
      let req =
            QB.TorrentInfoRequest
              { filter_ = Nothing,
                category = Nothing,
                tag = Just tag,
                hashes = Nothing
              }
      QB.getTorrents (Just req)

  GetRenameTorrents ->
    runQB cfg $ do
      let req =
            QB.TorrentInfoRequest
              { filter_ = Nothing,
                category = Nothing,
                tag = Just renameTag,
                hashes = Nothing
              }
      QB.getTorrents (Just req)

  StopTorrents hashes ->
    runQB cfg $ void $ QB.stopTorrents (map QB.InfoHash hashes)

  DeleteTorrents hashes deleteFiles ->
    runQB cfg $ void $ QB.deleteTorrents (map QB.InfoHash hashes) deleteFiles

  AddTagsToTorrents hashes tags ->
    runQB cfg $ void $ QB.addTags (map QB.InfoHash hashes) tags

  RemoveTagsFromTorrents hashes tags ->
    runQB cfg $ void $ QB.removeTags (map QB.InfoHash hashes) tags

-- | Run a qBittorrent action with login, converting errors to MoeError.
runQB ::
  (Error MoeError :> es, IOE :> es) =>
  DownloaderConfig ->
  Eff '[QB.QBittorrent, Error QB.QBError, IOE] a ->
  Eff es a
runQB cfg action = do
  let qbConfig = toQBConfig cfg
  client <- liftIO $ QB.newClient qbConfig
  result <-
    liftIO $
      runEff $
        runErrorNoCallStack @QB.QBError $
          QB.runQBittorrent client $ do
            void $ QB.login qbConfig
            action
  case result of
    Left e -> throwError $ DownloaderError $ show e
    Right a -> pure a

-- | Convert DownloaderConfig to QBConfig.
toQBConfig :: DownloaderConfig -> QB.QBConfig
toQBConfig cfg =
  let (host, port, useTLS) = parseUrl cfg.url
   in QB.QBConfig
        { host = host,
          port = port,
          username = cfg.username,
          password = cfg.password,
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
