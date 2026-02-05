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
  AddTorrent url savePath mTags ->
    runQB cfg $ do
      let allTags = case mTags of
            Nothing -> TagList [Moe, Rename]
            Just (TagList ts) -> TagList (ordNub $ [Moe, Rename] <> ts)
          req =
            QB.AddTorrentRequest
              { urls = Just url,
                savepath = savePath,
                category = Nothing,
                tags = Just $ toText allTags,
                rename = Nothing,
                stopped = Nothing
              }
      void $ QB.addTorrent req

  GetTorrentsByHashes hashes ->
    runQB cfg $ do
      let hashesText = T.intercalate "|" hashes
          req =
            QB.TorrentInfoRequest
              { filter_ = Nothing,
                category = Nothing,
                tag = Nothing,
                hashes = if null hashes then Nothing else Just hashesText
              }
      QB.getTorrents (Just req)

  GetTorrentsWithTag tag ->
    runQB cfg $ do
      let req =
            QB.TorrentInfoRequest
              { filter_ = Nothing,
                category = Nothing,
                tag = Just $ toText tag,
                hashes = Nothing
              }
      QB.getTorrents (Just req)

  StopTorrents hashes ->
    runQB cfg $ void $ QB.stopTorrents hashes

  DeleteTorrents hashes deleteFiles ->
    runQB cfg $ void $ QB.deleteTorrents hashes deleteFiles

  AddTagsToTorrents hashes (TagList tags) ->
    runQB cfg $ do
      let tagTexts = map toText tags
      void $ QB.addTags hashes tagTexts

  RemoveTagsFromTorrents hashes (TagList tags) ->
    runQB cfg $ do
      let tagTexts = map toText tags
      void $ QB.removeTags hashes tagTexts

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
