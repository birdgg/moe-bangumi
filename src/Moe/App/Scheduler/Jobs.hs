module Moe.App.Scheduler.Jobs
  ( defaultJobs,
    rssSyncJob,
    calendarSyncJob,
  )
where

import Effectful.Log (Logger)
import Moe.App.Env (MoeEnv, SchedulerConfig (..))
import Moe.App.Scheduler (JobDefinition)
import Moe.App.Scheduler.Jobs.CalendarSync (calendarSyncJob)
import Moe.App.Scheduler.Jobs.RssSync (rssSyncJob)

defaultJobs :: SchedulerConfig -> MoeEnv -> Logger -> [JobDefinition]
defaultJobs cfg env logger =
  [ rssSyncJob cfg.rssSyncCron env logger,
    calendarSyncJob cfg.calendarSyncCron env logger
  ]
