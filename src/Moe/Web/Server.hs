module Moe.Web.Server
  ( runMoe,
    runServer,
  )
where

import Control.Exception (bracket)
import Control.Exception.Backtrace (BacktraceMechanism (..), setBacktraceMechanismState)
import Control.Exception.Safe qualified as Safe
import Control.Monad.Except qualified as Except
import Data.ByteString.Lazy qualified as LBS
import Data.OpenApi (OpenApi)
import Data.Text.Display (display)
import Data.Text.IO qualified as TIO
import Effectful
import Moe.Prelude
import Effectful.Concurrent (Concurrent, forkIO, runConcurrent)
import Effectful.Dispatch.Static (unsafeEff_)
import Effectful.Error.Static (runErrorWith)
import Effectful.Log (Logger)
import Effectful.Log qualified as Log
import Effectful.Reader.Static qualified as Reader
import Effectful.Sqlite (SqliteDb (..), runSqlite)
import Moe.App.Bootstrap (bootstrap)
import Moe.App.CalendarSync (runInitialSeasonSync)
import Moe.App.Env (MoeConfig (..), MoeEnv (..), destroyDbPool, getSettingPath)
import Moe.App.Logging (LogConfig (..), makeLogger, runLog)
import Moe.App.Scheduler (JobDefinition, startScheduler)
import Moe.App.Scheduler.Jobs (defaultJobs)
import Moe.Error (MoeError (..))
import Moe.Infrastructure.BangumiData.Effect (runBangumiDataHttp)
import Moe.Infrastructure.Metadata.Effect (runMetadataHttp)
import Moe.Infrastructure.Setting.Effect (runSettingTVar)
import Moe.Web.API.Routes qualified as API
import Moe.Web.API.Server qualified as API
import Moe.Web.Routers
import Moe.Web.Scalar (scalarHtml)
import Moe.Web.Types
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
  bracket
    (bootstrap & runConcurrent & runEff)
    shutdownMoe
    ( \env ->
        runEff . withUnliftStrategy (ConcUnlift Ephemeral Unlimited) . runConcurrent $ do
          let baseURL = "http://localhost:" <> display env.config.port
          liftIO $ TIO.putStrLn $ "Starting server on " <> baseURL
          let withLogger = makeLogger env.config.logConfig.destination
          withLogger $ \appLogger ->
            provideCallStack $ runServer appLogger env
    )

shutdownMoe :: MoeEnv -> IO ()
shutdownMoe = destroyDbPool

logException :: Text -> MoeEnv -> Logger -> Safe.SomeException -> IO ()
logException component env logger exception =
  runEff $
    runLog component logger env.config.logConfig.logLevel $
      Log.logAttention_ $
        component <> " crashed: " <> show exception

runServer :: (RequireCallStack, Concurrent :> es, IOE :> es) => Logger -> MoeEnv -> Eff es ()
runServer logger env = do
  void $
    forkIO $
      unsafeEff_ $
        Safe.withException
          (runInitialSeasonSync logger env)
          (logException "initial-sync" env logger)
  let jobs = defaultJobs env logger
  void $
    forkIO $
      unsafeEff_ $
        Safe.withException
          (runScheduler logger env jobs)
          (logException "scheduler" env logger)
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
        & runMetadataHttp env.httpManager
        & runBangumiDataHttp env.httpManager
        & runSettingTVar env.settingVar (getSettingPath env)
        & runErrorWith
          ( \_callstack moeErr -> do
              Log.logAttention_ $ "Application error: " <> show moeErr
              pure . Left $ moeErrorToServerError moeErr
          )
        & runErrorWith (\_callstack err -> pure . Left $ err)
        & runLog "moe-server" logger env.config.logConfig.logLevel
        & runSqlite (DbPool env.dbPool)
        & runConcurrent
        & Reader.runReader env
        & runEff
  either Except.throwError pure result

moeErrorToServerError :: MoeError -> ServerError
moeErrorToServerError = \case
  DatabaseError msg -> err500 {errBody = LBS.fromStrict $ encodeUtf8 msg}
  NotFound msg -> err404 {errBody = LBS.fromStrict $ encodeUtf8 msg}
  ValidationError msg -> err400 {errBody = LBS.fromStrict $ encodeUtf8 msg}
  ExternalApiError msg -> err502 {errBody = LBS.fromStrict $ encodeUtf8 msg}

openApiHandler :: OpenApi
openApiHandler =
  toOpenApi (Proxy @API.Routes)
