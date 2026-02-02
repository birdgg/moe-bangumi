module Moe.Adapter.Scheduler.Jobs
  ( defaultJobs,
    rssSyncJob,
    calendarSyncJob,
  )
where

import Effectful.Log (Logger)
import Moe.Adapter.Scheduler.Jobs.CalendarSync (calendarSyncJob)
import Moe.Adapter.Scheduler.Jobs.RssSync (rssSyncJob)
import Moe.App.Env (MoeEnv, SchedulerConfig (..))
import Moe.App.Scheduler (JobDefinition)

defaultJobs :: SchedulerConfig -> MoeEnv -> Logger -> [JobDefinition]
defaultJobs cfg env logger =
  [ rssSyncJob cfg.rssSyncCron env logger,
    calendarSyncJob cfg.calendarSyncCron env logger
  ]
