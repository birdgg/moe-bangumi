module Moe.Infrastructure.Download.QBittorrent
  ( runDownload,
  )
where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read qualified as TR
import Effectful
import Effectful.Dispatch.Dynamic (interpret, localSeqUnlift)
import Effectful.Error.Static (throwError)
import Moe.Domain.Setting.Types (DownloaderConfig (..), UserPreference (..))
import Moe.Infrastructure.Download.Effect (Download (..), DownloadError (..))
import Moe.Infrastructure.Setting.Effect (Setting, getSetting)
import Network.HTTP.Client (Manager)
import Network.QBittorrent.Client
  ( AddTorrentRequest (..),
    QBConfig (..),
    mkClientEnvWithCookies,
    newCookieJar,
    runClientM,
  )
import Network.QBittorrent.Client qualified as QB

runDownload ::
  (Setting :> es, IOE :> es) =>
  Manager ->
  Eff (Download : es) a ->
  Eff es a
runDownload manager = interpret $ \env -> \case
  AddTorrent url savePath -> do
    pref <- getSetting
    case pref.downloader of
      Nothing -> localSeqUnlift env $ \unlift -> unlift $ throwError ConfigMissing
      Just cfg -> do
        result <- liftIO $ executeDownload manager cfg url savePath
        case result of
          Left err -> localSeqUnlift env $ \unlift -> unlift $ throwError err
          Right () -> pure ()

executeDownload ::
  Manager ->
  DownloaderConfig ->
  Text ->
  Maybe Text ->
  IO (Either DownloadError ())
executeDownload manager cfg url savePath = do
  cookieJar <- newCookieJar
  let qbConfig = toQBConfig cfg
      env = mkClientEnvWithCookies manager qbConfig cookieJar

  loginResult <- runClientM (QB.login qbConfig) env
  case loginResult of
    Left err -> pure $ Left $ LoginFailed $ T.pack $ show err
    Right response
      | response /= "Ok." -> pure $ Left $ LoginFailed response
      | otherwise -> do
          let req =
                AddTorrentRequest
                  { urls = Just url,
                    savepath = savePath,
                    category = Nothing,
                    tags = Nothing,
                    rename = Nothing,
                    stopped = Nothing
                  }
          addResult <- runClientM (QB.addTorrent req) env
          case addResult of
            Left err -> pure $ Left $ AddTorrentFailed $ T.pack $ show err
            Right _ -> pure $ Right ()

toQBConfig :: DownloaderConfig -> QBConfig
toQBConfig cfg =
  let (host, port, useTLS) = parseUrl cfg.url
   in QBConfig
        { host = host,
          port = port,
          username = cfg.username,
          password = cfg.password,
          useTLS = useTLS
        }

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
