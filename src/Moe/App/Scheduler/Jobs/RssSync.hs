module Moe.App.Scheduler.Jobs.RssSync
  ( rssSyncJob,
  )
where

import Data.Text (Text)
import Effectful
import Effectful.Log (Logger)
import Moe.App.Env (MoeEnv (..))
import Moe.App.Job (RssJobEffects, runRssJob)
import Moe.App.RssSync.Pipeline (runPipeline)
import Moe.App.Scheduler (JobDefinition (..))
import Moe.Domain.Scheduler.Types (JobResult (..), mkJobConfig)

defaultCron :: Text
defaultCron = "*/5 * * * *"

rssSyncJob :: MoeEnv -> Logger -> JobDefinition
rssSyncJob env logger =
  case mkJobConfig "rss-sync" defaultCron of
    Left err -> error $ "Invalid rss-sync cron: " <> show err
    Right config ->
      JobDefinition
        { jobConfig = config,
          jobAction = runRssJob "rss-sync" logger env (rssSyncAction env)
        }

rssSyncAction :: MoeEnv -> Eff RssJobEffects JobResult
rssSyncAction env = do
  _ <- runPipeline env.httpManager
  pure JobSuccess
