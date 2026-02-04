module Moe.App.Scheduler.Jobs.Cleanup
  ( cleanupJob,
  )
where

import Data.Text qualified as T
import Effectful
import Effectful.Log (Logger)
import Moe.App.Env (MoeEnv)
import Moe.App.Job (CleanupJobEffects, runCleanupJob)
import Moe.App.Scheduler (JobDefinition (..))
import Moe.App.Subscription.Cleanup (runCleanup)
import Moe.Domain.Scheduler.Types (JobResult (..), mkJobConfig)
import Moe.Prelude
import Prelude qualified

-- | Default cron: every hour
defaultCron :: Text
defaultCron = "0 * * * *"

cleanupJob :: MoeEnv -> Logger -> JobDefinition
cleanupJob env logger =
  case mkJobConfig "cleanup" defaultCron of
    Left err -> Prelude.error $ "Invalid cleanup cron: " <> T.unpack err
    Right config ->
      JobDefinition
        { jobConfig = config,
          jobAction = runCleanupJob "cleanup" logger env (cleanupAction env)
        }

cleanupAction :: MoeEnv -> Eff CleanupJobEffects JobResult
cleanupAction _ = do
  runCleanup
  pure JobSuccess
