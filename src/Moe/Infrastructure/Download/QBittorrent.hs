module Moe.Infrastructure.Download.QBittorrent
  ( runDownload,
  )
where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read qualified as TR
import Relude (toText, (<|>))
import Effectful
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Error.Static (Error, throwError)
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
  (Setting :> es, IOE :> es, Error DownloadError :> es) =>
  Manager ->
  Eff (Download : es) a ->
  Eff es a
runDownload manager = interpret $ \_ -> \case
  Login -> withConfig $ \cfg -> liftIO $ executeLogin manager cfg
  AddTorrent url savePath -> withConfig $ \cfg -> liftIO $ executeDownload manager cfg url savePath
  where
    withConfig action = do
      pref <- getSetting
      case pref.downloader of
        Nothing -> throwError ConfigMissing
        Just cfg -> case validateConfig cfg of
          Just err -> throwError err
          Nothing -> do
            result <- action cfg
            case result of
              Left err -> throwError err
              Right a -> pure a

validateConfig :: DownloaderConfig -> Maybe DownloadError
validateConfig cfg =
  checkEmpty "url" cfg.url
    <|> checkEmpty "username" cfg.username
    <|> checkEmpty "password" cfg.password
  where
    checkEmpty name value
      | T.null value = Just $ InvalidConfig name
      | otherwise = Nothing

executeLogin ::
  Manager ->
  DownloaderConfig ->
  IO (Either DownloadError ())
executeLogin manager cfg = do
  cookieJar <- newCookieJar
  let qbConfig = toQBConfig cfg
      env = mkClientEnvWithCookies manager qbConfig cookieJar
  loginResult <- runClientM (QB.login qbConfig) env
  pure $ case loginResult of
    Left err -> Left $ LoginFailed $ toText $ show err
    Right response
      | response /= "Ok." -> Left $ LoginFailed response
      | otherwise -> Right ()

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
    Left err -> pure $ Left $ LoginFailed $ toText $ show err
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
            Left err -> pure $ Left $ AddTorrentFailed $ toText $ show err
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
