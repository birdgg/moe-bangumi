module Moe.App.Job.Runner
  ( runMetadataJob,
    runRssJob,
    runSubscriptionJob,
    runCleanupJob,
  )
where

import Effectful (Eff)
import Effectful.Log (Logger)
import Moe.App.Effect.Runner
  ( runCleanupEffects,
    runMetadataEffects,
    runRssEffects,
    runSubscriptionEffects,
  )
import Moe.App.Env (MoeEnv)
import Moe.App.Job.Types (CleanupJobEffects, MetadataJobEffects, RssJobEffects, SubscriptionJobEffects)
import Moe.Domain.Scheduler.Types (JobResult)
import Moe.Prelude

runMetadataJob ::
  Text ->
  Logger ->
  MoeEnv ->
  Eff MetadataJobEffects JobResult ->
  IO JobResult
runMetadataJob logPrefix logger env = runMetadataEffects env logger logPrefix

runRssJob ::
  Text ->
  Logger ->
  MoeEnv ->
  Eff RssJobEffects JobResult ->
  IO JobResult
runRssJob logPrefix logger env = runRssEffects env logger logPrefix

runSubscriptionJob ::
  Text ->
  Logger ->
  MoeEnv ->
  Eff SubscriptionJobEffects JobResult ->
  IO JobResult
runSubscriptionJob logPrefix logger env = runSubscriptionEffects env logger logPrefix

runCleanupJob ::
  Text ->
  Logger ->
  MoeEnv ->
  Eff CleanupJobEffects JobResult ->
  IO JobResult
runCleanupJob logPrefix logger env = runCleanupEffects env logger logPrefix
