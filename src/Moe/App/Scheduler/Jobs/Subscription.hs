module Moe.App.Scheduler.Jobs.Subscription
  ( subscriptionJob,
  )
where

import Data.Text (Text)
import Effectful
import Effectful.Log (Logger)
import Moe.App.Env (MoeEnv (..))
import Moe.App.Job (RssJobEffects, runRssJob)
import Moe.App.Subscription.Pipeline (runPipeline)
import Moe.App.Scheduler (JobDefinition (..))
import Moe.Domain.Scheduler.Types (JobResult (..), mkJobConfig)

subscriptionJob :: Text -> MoeEnv -> Logger -> JobDefinition
subscriptionJob cronExpr env logger =
  case mkJobConfig "subscription" cronExpr of
    Left err -> error $ "Invalid subscription cron: " <> show err
    Right config ->
      JobDefinition
        { jobConfig = config,
          jobAction = runRssJob "subscription" logger env (subscriptionAction env)
        }

subscriptionAction :: MoeEnv -> Eff RssJobEffects JobResult
subscriptionAction env = do
  runPipeline env.httpManager
  pure JobSuccess
