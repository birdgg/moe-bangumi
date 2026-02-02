module Moe.App.Scheduler
  ( JobDefinition (..),
    SchedulerHandle (..),
    SchedulerState (..),
    startScheduler,
    stopScheduler,
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async)
import Control.Concurrent.Async qualified as Async
import Control.Exception (SomeException)
import Control.Exception qualified as Exception
import Control.Monad (when)
import Data.Foldable (traverse_)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Display (display)
import Data.Text.IO qualified as TIO
import Data.Time (NominalDiffTime, diffUTCTime, getCurrentTime)
import Effectful
import Effectful.Log qualified as Log
import Moe.Domain.Scheduler.Types
import System.Cron (nextMatch)
import System.IO (hFlush, stdout)

data JobDefinition = JobDefinition
  { jobConfig :: JobConfig,
    jobAction :: IO JobResult
  }

data SchedulerState = SchedulerState
  { running :: Bool,
    jobs :: [ScheduledJob]
  }
  deriving stock (Eq, Show)

data SchedulerHandle = SchedulerHandle
  { stateRef :: IORef SchedulerState,
    asyncJobs :: [Async ()]
  }

startScheduler :: (IOE :> es, Log.Log :> es) => [JobDefinition] -> Eff es SchedulerHandle
startScheduler jobDefs = do
  let enabledJobs = filter (\j -> j.jobConfig.enabled) jobDefs
  let scheduledJobs = map initScheduledJob enabledJobs
  stateRef <- liftIO $ newIORef SchedulerState {running = True, jobs = scheduledJobs}
  asyncJobs <- traverse (runJobAsync stateRef) enabledJobs
  pure SchedulerHandle {stateRef, asyncJobs}

stopScheduler :: (IOE :> es, Log.Log :> es) => SchedulerHandle -> Eff es ()
stopScheduler handle = do
  Log.logInfo_ "Stopping scheduler..."
  liftIO $ atomicModifyIORef' handle.stateRef (\s -> (s {running = False}, ()))
  liftIO $ traverse_ Async.cancel handle.asyncJobs
  Log.logInfo_ "Scheduler stopped"

initScheduledJob :: JobDefinition -> ScheduledJob
initScheduledJob def =
  ScheduledJob
    { config = def.jobConfig,
      lastRun = Nothing,
      lastStatus = JobIdle,
      runCount = 0
    }

runJobAsync :: (IOE :> es, Log.Log :> es) => IORef SchedulerState -> JobDefinition -> Eff es (Async ())
runJobAsync stateRef def = do
  let cfg = def.jobConfig
  now <- liftIO getCurrentTime
  case nextMatch cfg.schedule now of
    Nothing -> do
      Log.logAttention_ $ "[" <> display cfg.jobId <> "] No upcoming schedule match"
      liftIO $ Async.async $ pure ()
    Just _ ->
      liftIO $ Async.async $ runJobLoop stateRef def

runJobLoop :: IORef SchedulerState -> JobDefinition -> IO ()
runJobLoop stateRef def = loop
  where
    cfg = def.jobConfig

    loop = do
      st <- readIORef stateRef
      when st.running $ do
        now <- getCurrentTime
        case nextMatch cfg.schedule now of
          Nothing -> pure ()
          Just nextRun -> do
            let delay = diffUTCTime nextRun now
            when (delay > 0) $
              sleepUntil delay stateRef
            st' <- readIORef stateRef
            when st'.running $ do
              executeJob stateRef def
              loop

sleepUntil :: NominalDiffTime -> IORef SchedulerState -> IO ()
sleepUntil delay stateRef = go (ceiling (delay * 1_000_000) :: Integer)
  where
    maxSleep = 60_000_000
    go remaining
      | remaining <= 0 = pure ()
      | otherwise = do
          st <- readIORef stateRef
          when st.running $ do
            let sleepTime = min remaining maxSleep
            threadDelay (fromIntegral sleepTime)
            go (remaining - sleepTime)

executeJob :: IORef SchedulerState -> JobDefinition -> IO ()
executeJob stateRef def = do
  let cfg = def.jobConfig
  logJob cfg.jobId "Starting scheduled task"
  updateJobStatus stateRef cfg.jobId JobRunning
  result <- def.jobAction `Exception.catch` handleException
  now <- getCurrentTime
  let newStatus = case result of
        JobSuccess -> JobCompleted
        JobFailure msg -> JobFailed msg
  updateJobState stateRef cfg.jobId $ \job ->
    job
      { lastRun = Just now,
        lastStatus = newStatus,
        runCount = job.runCount + 1
      }
  logJob cfg.jobId $ "Finished: " <> display result
  where
    handleException :: SomeException -> IO JobResult
    handleException e = pure $ JobFailure (T.pack (show e))

logJob :: JobId -> Text -> IO ()
logJob jid msg = do
  TIO.putStrLn $ "[" <> display jid <> "] " <> msg
  hFlush stdout

updateJobStatus :: IORef SchedulerState -> JobId -> JobStatus -> IO ()
updateJobStatus stateRef jid status =
  updateJobState stateRef jid $ \job -> job {lastStatus = status}

updateJobState :: IORef SchedulerState -> JobId -> (ScheduledJob -> ScheduledJob) -> IO ()
updateJobState stateRef jid f =
  atomicModifyIORef' stateRef $ \s ->
    let jobs' = map updateIfMatch s.jobs
     in (s {jobs = jobs'}, ())
  where
    updateIfMatch job
      | job.config.jobId == jid = f job
      | otherwise = job
