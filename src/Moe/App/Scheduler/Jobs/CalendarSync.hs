module Moe.App.Scheduler.Jobs.CalendarSync
  ( calendarSyncJob,
  )
where

import Data.Text qualified as T
import Effectful.Log (Logger)
import Moe.App.CalendarSync (runMonthlySync)
import Moe.App.Env (MoeEnv)
import Moe.App.Scheduler (JobDefinition (..))
import Moe.Domain.Scheduler.Types (JobResult (..), mkJobConfig)
import Moe.Prelude
import Prelude qualified

defaultCron :: Text
defaultCron = "0 6 1-10 * *"

calendarSyncJob :: MoeEnv -> Logger -> JobDefinition
calendarSyncJob env logger =
  case mkJobConfig "calendar-sync" defaultCron of
    Left err -> Prelude.error $ "Invalid calendar-sync cron: " <> T.unpack err
    Right config ->
      JobDefinition
        { jobConfig = config,
          jobAction = calendarSyncAction env logger
        }

calendarSyncAction :: MoeEnv -> Logger -> IO JobResult
calendarSyncAction env logger = do
  runMonthlySync logger env
  pure JobSuccess
