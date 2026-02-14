module Moe.Web.Server
  ( runServer,
    mkServer,
    moeServer,
    naturalTransform,
    openApiHandler,
  )
where

import Control.Monad.Except qualified as Except
import Data.Aeson qualified as Aeson
import Data.OpenApi (OpenApi)
import Effectful
import Effectful.Concurrent (runConcurrent)
import Effectful.Dispatch.Static ()
import Effectful.Error.Static (runErrorWith)
import Effectful.FileSystem (runFileSystem)
import Effectful.Log (Logger)
import Effectful.Log qualified as Log
import Effectful.Reader.Static (runReader)
import Effectful.Sqlite (SqliteDb (..), runSqlite)
import Moe.App.Env (MoeConfig (..), MoeEnv (..))
import Moe.App.Logging (LogConfig (..), runLog)
import Data.Text.Display (display)
import Moe.Error (AppError (..))
import Moe.Infra.Downloader.Adapter (runDownloaderQBittorrent)
import Moe.Infra.Metadata.Effect (runMetadataHttp)
import Moe.Infra.Rss.Effect (runRss)
import Moe.Infra.Setting.Effect (runSetting, runSettingWriter)
import Moe.Prelude
import Moe.Web.API.Routes qualified as API
import Moe.Web.API.Server qualified as API
import Moe.Web.Routers
import Moe.Web.Types
import Network.Wai.Handler.Warp
  ( defaultSettings,
    runSettings,
    setPort,
  )
import RequireCallStack (RequireCallStack)
import Servant
  ( Application,
    Context ((:.), EmptyContext),
    ErrorFormatters (..),
    Handler,
    ServerError (..),
    defaultErrorFormatters,
    err400,
    err404,
    err500,
    err502,
    serveWithContextT,
  )
import Servant.OpenApi
import Servant.Scalar (scalarUIServer')
import Servant.Server.StaticFiles (serveDirectoryWith)
import Network.Wai (responseFile)
import Network.HTTP.Types (status200)
import WaiAppStatic.Storage.Filesystem (defaultFileServerSettings)
import WaiAppStatic.Types (StaticSettings (..))
import Servant.Server.Generic (AsServerT)

runServer :: (RequireCallStack, IOE :> es) => Logger -> MoeEnv -> Eff es ()
runServer logger env = do
  let warpSettings = setPort env.config.port defaultSettings
  liftIO $ runSettings warpSettings (mkServer env logger)

mkServer :: (RequireCallStack) => MoeEnv -> Logger -> Application
mkServer env logger =
  serveWithContextT
    (Proxy @ServerRoutes)
    (jsonErrorFormatters :. EmptyContext)
    (naturalTransform env logger)
    moeServer

moeServer :: (RequireCallStack) => Routes (AsServerT ServerEff)
moeServer =
  Routes
    { api = API.apiServer,
      doc = scalarUIServer' (pure openApiHandler),
      spa = serveDirectoryWith spaSettings
    }

spaSettings :: StaticSettings
spaSettings =
  let baseSettings = defaultFileServerSettings "web/dist"
   in baseSettings{ss404Handler = Just spaFallback}

spaFallback :: Application
spaFallback _request respond =
  respond $ responseFile status200 [("Content-Type", "text/html")] "web/dist/index.html" Nothing

naturalTransform :: (RequireCallStack) => MoeEnv -> Logger -> ServerEff a -> Handler a
naturalTransform env logger app = do
  result <-
    liftIO $
      Right
        <$> app
        & runDownloaderQBittorrent env.downloaderEnv env.httpManager
        & runRss env.httpManager
        & runMetadataHttp env.httpManager
        & runSetting env.settingEnv
        & runSettingWriter env.settingEnv
        & runSqlite (DbPool env.dbPool)
        & runErrorWith
          ( \_callstack moeErr -> do
              Log.logAttention_ $ display moeErr
              pure . Left $ appErrorToServerError moeErr
          )
        & runLog "moe-server" logger env.config.logConfig.logLevel
        & runReader env
        & runConcurrent
        & runFileSystem
        & runEff
  either Except.throwError pure result

jsonErrorFormatters :: ErrorFormatters
jsonErrorFormatters =
  defaultErrorFormatters
    { bodyParserErrorFormatter = \_ _ msg -> jsonError err400 (toText msg),
      urlParseErrorFormatter = \_ _ msg -> jsonError err400 (toText msg)
    }

appErrorToServerError :: AppError -> ServerError
appErrorToServerError = \case
  RssError err -> jsonError err502 (display err)
  DownloaderError err -> jsonError err502 (display err)
  MetadataError err -> jsonError err502 (display err)
  DatabaseError err -> jsonError err500 (display err)
  NotFound msg -> jsonError err404 msg
  ValidationError msg -> jsonError err400 msg

jsonError :: ServerError -> Text -> ServerError
jsonError base msg =
  base
    { errBody = Aeson.encode $ Aeson.object ["message" Aeson..= msg],
      errHeaders = [("Content-Type", "application/json")]
    }

openApiHandler :: OpenApi
openApiHandler =
  toOpenApi (Proxy @API.Routes)
