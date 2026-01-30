module Moe.Web.Server
  ( runMoe,
    runServer,
  )
where

import Control.Exception.Backtrace (BacktraceMechanism (..), setBacktraceMechanismState)
import Control.Exception.Safe qualified as Safe
import Control.Monad.Except qualified as Except
import Data.OpenApi (OpenApi)
import Data.Text.Display (display)
import Effectful
import Effectful.Concurrent (Concurrent, forkIO, runConcurrent)
import Effectful.Dispatch.Static (unsafeEff_)
import Effectful.Error.Static (runErrorWith)
import Effectful.Log (Logger)
import Effectful.Log qualified as Log
import Effectful.Reader.Static qualified as Reader
import Effectful.Sqlite (SqliteDb (..), runSqlite)
import Moe.Adapter.File.Setting (runSettingFile)
import Moe.Adapter.Http.Metadata (runMetadataHttp)
import Moe.Adapter.Scheduler.Jobs (defaultJobs)
import Moe.App.Env (MoeConfig (..), MoeEnv (..), SchedulerConfig (..), getDatabasePath, getSettingPath, mkMoeEnv, parseMoeConfig)
import Moe.App.Error (MoeError (..))
import Moe.App.Logging (LogConfig (..), makeLogger, runLog)
import Moe.App.Scheduler (JobDefinition, startScheduler)
import Moe.Web.API.Routes qualified as API
import Moe.Web.API.Server qualified as API
import Moe.Web.Routers
import Moe.Web.Scalar (scalarHtml)
import Moe.Web.Types
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.Wai.Handler.Warp
  ( defaultSettings,
    runSettings,
    setPort,
  )
import RequireCallStack (RequireCallStack, provideCallStack)
import Servant
  ( Application,
    Context (EmptyContext),
    Handler,
    ServerError (..),
    err400,
    err404,
    err500,
    err502,
    serveWithContextT,
  )
import Servant.OpenApi
import Servant.Server.Generic (AsServerT)

runMoe :: IO ()
runMoe = do
  setBacktraceMechanismState HasCallStackBacktrace True
  env <- parseMoeConfig >>= mkMoeEnv
  runEff . withUnliftStrategy (ConcUnlift Ephemeral Unlimited) . runConcurrent $ do
    let baseURL = "http://localhost:" <> display env.config.port
    liftIO $ putTextLn $ "Starting server on " <> baseURL
    let withLogger = makeLogger env.config.logConfig.destination
    withLogger $ \appLogger ->
      provideCallStack $ runServer appLogger env

logSchedulerException :: MoeEnv -> Logger -> Safe.SomeException -> IO ()
logSchedulerException env logger exception =
  runEff $
    runLog "scheduler" logger env.config.logConfig.logLevel $
      Log.logAttention_ $
        "Scheduler crashed: " <> show exception

runServer :: (RequireCallStack, Concurrent :> es, IOE :> es) => Logger -> MoeEnv -> Eff es ()
runServer logger env = do
  _httpManager <- liftIO $ HTTP.newManager tlsManagerSettings
  let schedulerCfg = env.config.schedulerConfig
  when schedulerCfg.enableScheduler $ do
    let jobs = defaultJobs schedulerCfg
    void $
      forkIO $
        unsafeEff_ $
          Safe.withException
            (runScheduler logger env jobs)
            (logSchedulerException env logger)
  let warpSettings = setPort env.config.port defaultSettings
  liftIO $ runSettings warpSettings (mkServer env logger)

runScheduler :: Logger -> MoeEnv -> [JobDefinition] -> IO ()
runScheduler logger env jobs =
  runEff $
    runLog "scheduler" logger env.config.logConfig.logLevel $
      void $
        startScheduler jobs

mkServer :: (RequireCallStack) => MoeEnv -> Logger -> Application
mkServer env logger =
  serveWithContextT
    (Proxy @ServerRoutes)
    EmptyContext
    (naturalTransform env logger)
    moeServer

moeServer :: (RequireCallStack) => Routes (AsServerT MoeEff)
moeServer =
  Routes
    { api = API.apiServer,
      doc = docServer
    }

docServer :: DocRoutes (AsServerT MoeEff)
docServer =
  DocRoutes
    { docUI = pure scalarHtml,
      openApiSpec = pure openApiHandler
    }

naturalTransform :: (RequireCallStack) => MoeEnv -> Logger -> MoeEff a -> Handler a
naturalTransform env logger app = do
  result <-
    liftIO $
      Right
        <$> app
          & runMetadataHttp
          & runSettingFile (getSettingPath env)
          & runErrorWith
            ( \_callstack moeErr -> do
                Log.logAttention_ $ "Application error: " <> show moeErr
                pure . Left $ moeErrorToServerError moeErr
            )
          & runErrorWith (\_callstack err -> pure . Left $ err)
          & runLog "moe-server" logger env.config.logConfig.logLevel
          & runSqlite (DbFile $ getDatabasePath env)
          & Reader.runReader env
          & runEff
  either Except.throwError pure result

moeErrorToServerError :: MoeError -> ServerError
moeErrorToServerError = \case
  DatabaseError msg -> err500 {errBody = encodeUtf8 msg}
  NotFound msg -> err404 {errBody = encodeUtf8 msg}
  ValidationError msg -> err400 {errBody = encodeUtf8 msg}
  ExternalApiError msg -> err502 {errBody = encodeUtf8 msg}

openApiHandler :: OpenApi
openApiHandler =
  toOpenApi (Proxy @API.Routes)
