module Moe.Web.Server
  ( runServer,
    mkServer,
    moeServer,
    naturalTransform,
  )
where

import Control.Monad.Except qualified as Except
import Data.Map.Strict qualified as Map
import Data.Tagged (Tagged (..))
import Data.Text qualified as T
import Data.Text.Display (Display, display)
import Effectful.Dispatch.Static ()
import Effectful.Log qualified as Log
import Moe.App.Env (MoeConfig (..), MoeEnv (..))
import Moe.App.Logging (LogConfig (..), runLog)
import Moe.Infra.Downloader.Adapter (runDownloaderQBittorrent)
import Moe.Infra.Metadata.Effect (runMetadataHttp)
import Moe.Infra.Rss.Effect (runRss)
import Moe.Infra.Setting.Effect (runSetting, runSettingWriter)
import Moe.Infra.Update.Adapter (runUpdateGitHub)
import Moe.Prelude hiding (type (:.)(..))
import Moe.Web.API.Server qualified as API
import Moe.Web.Error (jsonError, webErrorToServerError)
import Moe.Web.Embedded (embeddedFiles, indexHtml)
import Moe.Web.Routers
import Moe.Web.Types
import Network.HTTP.Types (status200)
import Network.Mime (defaultMimeLookup)
import Network.Wai (pathInfo, responseLBS)
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
    ServerError,
    defaultErrorFormatters,
    err400,
    err500,
    err502,
    serveWithContextT,
  )
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
      spa = Tagged embeddedSpa
    }

-- | Serve embedded frontend files from memory with SPA fallback.
embeddedSpa :: Application
embeddedSpa request respond = do
  let path = T.intercalate "/" (pathInfo request)
      filePath = toString path
      contentType = defaultMimeLookup (toText filePath)
  case Map.lookup filePath embeddedFiles of
    Just content ->
      respond $ responseLBS status200 [("Content-Type", contentType)] (fromStrict content)
    Nothing ->
      respond $ responseLBS status200 [("Content-Type", "text/html")] (fromStrict indexHtml)

naturalTransform :: (RequireCallStack) => MoeEnv -> Logger -> ServerEff a -> Handler a
naturalTransform env logger app = do
  result <-
    liftIO $
      Right
        <$> app
        & runUpdateGitHub env.updateEnv env.httpManager
        & resolveInfraError err500
        & runDownloaderQBittorrent env.downloaderEnv env.httpManager
        & resolveInfraError err502
        & runRss env.httpManager
        & resolveInfraError err502
        & runMetadataHttp env.httpManager
        & resolveInfraError err502
        & runSetting env.settingEnv
        & runSettingWriter env.settingEnv
        & runSqlite (DbPool env.dbPool)
        & resolveInfraError err500
        & runErrorWith
          ( \_cs webErr -> do
              Log.logAttention_ $ display webErr
              pure . Left $ webErrorToServerError webErr
          )
        & runLog "moe-server" logger env.config.logConfig.logLevel
        & runReader env
        & runConcurrent
        & runFileSystem
        & runEnvironment
        & runTime
        & runEff
  either Except.throwError pure result

-- | Resolve any infrastructure error effect by logging and converting to ServerError.
resolveInfraError ::
  forall e es a.
  (Show e, Display e, Log :> es) =>
  ServerError ->
  Eff (Error e : es) (Either ServerError a) ->
  Eff es (Either ServerError a)
resolveInfraError baseStatus =
  runErrorWith $ \_cs err -> do
    Log.logAttention_ $ display err
    pure . Left $ jsonError baseStatus (display err)

jsonErrorFormatters :: ErrorFormatters
jsonErrorFormatters =
  defaultErrorFormatters
    { bodyParserErrorFormatter = \_ _ msg -> jsonError err400 (toText msg),
      urlParseErrorFormatter = \_ _ msg -> jsonError err400 (toText msg)
    }
