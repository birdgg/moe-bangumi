-- | Calendar sync worker thread: periodically syncs season data.
-- Only performs sync on days 1-10 of each month.
module Moe.Job.CalendarSync.Worker
  ( calendarSyncWorkerThread,
  )
where

import Data.Text.Display (display)
import Data.Time.Calendar (toGregorian)
import Data.Time.Clock (utctDay)
import Effectful.Log qualified as Log
import Moe.App.Env (MoeEnv (..))
import Moe.Infra.Metadata.Effect (runMetadataHttp)
import Moe.Infra.Setting.Effect (runSetting)
import Moe.Job.CalendarSync.Process (runCalendarSync)
import Moe.Job.Effect (periodicWorker, runBaseEffects)
import Moe.Prelude

-- | Entry point for the calendar sync worker thread.
calendarSyncWorkerThread :: MoeEnv -> Logger -> IO ()
calendarSyncWorkerThread env logger =
  runBaseEffects env logger "CalendarSync" $
    runSetting env.settingEnv $
      runErrorWith (\_ err -> Log.logAttention_ $ display err) $
        runMetadataHttp env.httpManager $
          periodicWorker "CalendarSync" (24 * 60 * 60 * 1_000_000) guardedSync
 where
  guardedSync = do
    (_, _, day) <- toGregorian . utctDay <$> currentTime
    when (day >= 1 && day <= 10) runCalendarSync
