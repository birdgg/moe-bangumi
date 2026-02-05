module Moe.App.Scheduler.Jobs.Rename
  ( renameJob,
  )
where

import Effectful
import Effectful.Log (Logger)
import Moe.App.Env (MoeEnv)
import Moe.App.Job (RenameJobEffects, runRenameJob)
import Moe.App.Rename.Run (runRename)
import Moe.App.Scheduler (JobDefinition (..))
import Moe.Domain.Scheduler.Types (JobResult (..), mkJobConfig)
import Moe.Prelude
import Prelude qualified

-- | Default cron: every 15 minutes.
defaultCron :: Text
defaultCron = "*/15 * * * *"

renameJob :: MoeEnv -> Logger -> JobDefinition
renameJob env logger =
  case mkJobConfig "rename" defaultCron of
    Left err -> Prelude.error $ "Invalid rename cron: " <> toString err
    Right config ->
      JobDefinition
        { jobConfig = config,
          jobAction = runRenameJob "rename" logger env renameAction
        }

renameAction :: Eff RenameJobEffects JobResult
renameAction = do
  runRename
  pure JobSuccess
