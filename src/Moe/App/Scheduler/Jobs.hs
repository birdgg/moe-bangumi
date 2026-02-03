module Moe.App.Scheduler.Jobs
  ( defaultJobs,
    rssSyncJob,
    calendarSyncJob,
  )
where

import Effectful.Log (Logger)
import Moe.App.Env (MoeEnv)
import Moe.App.Scheduler (JobDefinition)
import Moe.App.Scheduler.Jobs.CalendarSync (calendarSyncJob)
import Moe.App.Scheduler.Jobs.RssSync (rssSyncJob)

defaultJobs :: MoeEnv -> Logger -> [JobDefinition]
defaultJobs env logger =
  [ rssSyncJob env logger,
    calendarSyncJob env logger
  ]
