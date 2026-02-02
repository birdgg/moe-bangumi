module Moe.Domain.Scheduler.Types
  ( JobId (..),
    JobConfig (..),
    JobStatus (..),
    ScheduledJob (..),
    JobResult (..),
    mkJobConfig,
    parseCronSchedule,
  )
where

import Data.Hashable (Hashable)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Display (Display (..))
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import System.Cron (CronSchedule)
import System.Cron.Parser (parseCronSchedule)

newtype JobId = JobId Text
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Hashable)

instance Display JobId where
  displayBuilder (JobId t) = displayBuilder t

data JobConfig = JobConfig
  { jobId :: JobId,
    schedule :: CronSchedule,
    enabled :: Bool
  }

instance Show JobConfig where
  show cfg = "JobConfig {jobId = " <> show cfg.jobId <> ", enabled = " <> show cfg.enabled <> "}"

instance Eq JobConfig where
  a == b = a.jobId == b.jobId && a.enabled == b.enabled

mkJobConfig :: Text -> Text -> Either Text JobConfig
mkJobConfig name cronExpr =
  case parseCronSchedule cronExpr of
    Left err -> Left $ "Invalid cron expression: " <> T.pack err
    Right sched ->
      Right
        JobConfig
          { jobId = JobId name,
            schedule = sched,
            enabled = True
          }

data JobStatus
  = JobIdle
  | JobRunning
  | JobFailed Text
  | JobCompleted
  deriving stock (Eq, Show)

instance Display JobStatus where
  displayBuilder JobIdle = "idle"
  displayBuilder JobRunning = "running"
  displayBuilder (JobFailed msg) = "failed: " <> displayBuilder msg
  displayBuilder JobCompleted = "completed"

data ScheduledJob = ScheduledJob
  { config :: JobConfig,
    lastRun :: Maybe UTCTime,
    lastStatus :: JobStatus,
    runCount :: Natural
  }
  deriving stock (Eq, Show)

data JobResult
  = JobSuccess
  | JobFailure Text
  deriving stock (Eq, Show)

instance Display JobResult where
  displayBuilder JobSuccess = "success"
  displayBuilder (JobFailure msg) = "failure: " <> displayBuilder msg
