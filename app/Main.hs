-- | Application lifecycle orchestration.
module Main (main) where

import Control.Exception (bracket)
import Control.Exception.Backtrace (BacktraceMechanism (..), setBacktraceMechanismState)
import Data.Text.Display (display)
import Moe.App.Bootstrap (bootstrap)
import Moe.App.Env (MoeConfig (..), MoeEnv (..), destroyDbPool)
import Moe.App.Logging (makeLogger)
import Moe.App.MemoryMonitor (memoryMonitorThread)
import Moe.Job.CalendarSync (calendarSyncWorkerThread)
import Moe.Job.Cleanup (cleanupWorkerThread)
import Moe.Job.Rename (renameWorkerThread)
import Moe.Job.Subscription (rssWorkerThread)
import Moe.Prelude
import Moe.Web.Server (runServer)
import RequireCallStack (provideCallStack)
import Supervisor (supervise)

main :: IO ()
main = do
  setBacktraceMechanismState HasCallStackBacktrace True
  args <- getArgs
  if "--supervised" `elem` args
    then runApp
    else supervise

runApp :: IO ()
runApp =
  bracket
    (bootstrap & runFileSystem & runConcurrent & runEff)
    shutdownMoe
    ( \env ->
        runEff . withUnliftStrategy (ConcUnlift Ephemeral Unlimited) . runConcurrent $ do
          let baseURL = "http://localhost:" <> display env.config.port
          liftIO $ putTextLn $ "Starting server on " <> baseURL
          let withLogger = makeLogger env.config.dataFolder
          withLogger $ \appLogger -> provideCallStack $ do
            withAsync (liftIO $ rssWorkerThread env appLogger) $ \_ ->
              withAsync (liftIO $ renameWorkerThread env appLogger) $ \_ ->
                withAsync (liftIO $ calendarSyncWorkerThread env appLogger) $ \_ ->
                  withAsync (liftIO $ cleanupWorkerThread env appLogger) $ \_ ->
                    withAsync (liftIO $ memoryMonitorThread env appLogger) $ \_ ->
                      runServer appLogger env
    )

shutdownMoe :: MoeEnv -> IO ()
shutdownMoe = destroyDbPool
