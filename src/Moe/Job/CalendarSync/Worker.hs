-- | Calendar sync worker thread: periodically syncs season data.
-- Replaces cron-based calendarSyncJob with a long-running thread that syncs on day 1-10 of each month.
module Moe.Job.CalendarSync.Worker
  ( calendarSyncWorkerThread,
  )
where

import Control.Exception (SomeAsyncException (..))
import Data.Time.Calendar (toGregorian)
import Data.Time.Clock (getCurrentTime, utctDay)
import Effectful
import Effectful.Concurrent.STM qualified as STM
import Effectful.Exception (throwIO, try)
import Effectful.Log (Logger)
import Effectful.Log qualified as Log
import Moe.App.Env (MoeEnv (..))
import Moe.Infra.Metadata.Effect (Metadata, runMetadataHttp)
import Moe.Infra.Setting.Effect (Setting, runSetting)
import Moe.Job.CalendarSync.Process (runCalendarSync)
import Moe.Job.Effect (runBaseEffects)
import Moe.Prelude

-- | Entry point for the calendar sync worker thread.
calendarSyncWorkerThread :: MoeEnv -> Logger -> IO ()
calendarSyncWorkerThread env logger =
  runBaseEffects env logger "CalendarSync" $
    runSetting env.settingEnv $
      runMetadataHttp env.httpManager calendarSyncWorkerLoop

-- | Main worker loop: runs sync on startup, then every 24 hours.
-- Only performs sync on days 1-10 of the month.
calendarSyncWorkerLoop ::
  (Metadata :> es, Setting :> es, Sqlite :> es, Concurrent :> es, Log :> es, IOE :> es) =>
  Eff es ()
calendarSyncWorkerLoop = do
  safeRunCalendarSync
  void $ infinitely $ do
    timerVar <- STM.registerDelay twentyFourHours
    STM.atomically $ STM.readTVar timerVar >>= STM.check
    safeRunCalendarSync

-- | Run calendar sync with day-of-month check and error recovery.
safeRunCalendarSync ::
  (Metadata :> es, Setting :> es, Sqlite :> es, Concurrent :> es, Log :> es, IOE :> es) =>
  Eff es ()
safeRunCalendarSync = do
  (_, _, day) <- liftIO $ toGregorian . utctDay <$> getCurrentTime
  if day >= 1 && day <= 10
    then do
      result <- try @SomeException runCalendarSync
      case result of
        Left ex
          | Just (SomeAsyncException _) <- fromException ex -> throwIO ex
          | otherwise -> Log.logAttention_ $ "CalendarSync worker error: " <> toText (displayException ex)
        Right () -> pass
    else pass

-- | Timer interval: 24 hours in microseconds.
twentyFourHours :: Int
twentyFourHours = 24 * 60 * 60 * 1_000_000
