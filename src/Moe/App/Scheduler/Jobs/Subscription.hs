module Moe.App.Scheduler.Jobs.Subscription
  ( subscriptionJob,
  )
where

import Data.Text qualified as T
import Effectful
import Effectful.Log (Logger)
import Moe.App.Env (MoeEnv (..))
import Moe.App.Job (SubscriptionJobEffects, runSubscriptionJob)
import Moe.App.Scheduler (JobDefinition (..))
import Moe.App.Subscription.Pipeline (runPipeline)
import Moe.Domain.Scheduler.Types (JobResult (..), mkJobConfig)
import Moe.Prelude
import Prelude qualified

subscriptionJob :: Text -> MoeEnv -> Logger -> JobDefinition
subscriptionJob cronExpr env logger =
  case mkJobConfig "subscription" cronExpr of
    Left err -> Prelude.error $ "Invalid subscription cron: " <> T.unpack err
    Right config ->
      JobDefinition
        { jobConfig = config,
          jobAction = runSubscriptionJob "subscription" logger env (subscriptionAction env)
        }

subscriptionAction :: MoeEnv -> Eff SubscriptionJobEffects JobResult
subscriptionAction env = do
  runPipeline env.httpManager
  pure JobSuccess
