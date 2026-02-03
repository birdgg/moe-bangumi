module Moe.App.Scheduler.Jobs
  ( defaultJobs,
    subscriptionJob,
    calendarSyncJob,
  )
where

import Data.Text (Text)
import Effectful.Log (Logger)
import Moe.App.Env (MoeEnv)
import Moe.App.Scheduler (JobDefinition)
import Moe.App.Scheduler.Jobs.CalendarSync (calendarSyncJob)
import Moe.App.Scheduler.Jobs.Subscription (subscriptionJob)

defaultCron :: Text
defaultCron = "*/5 * * * *"

defaultJobs :: MoeEnv -> Logger -> [JobDefinition]
defaultJobs env logger =
  [ subscriptionJob defaultCron env logger,
    calendarSyncJob env logger
  ]
