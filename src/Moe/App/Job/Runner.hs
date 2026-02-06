module Moe.App.Job.Runner
  ( runMetadataJob,
    runSubscriptionJob,
    runRenameJob,
  )
where

import Effectful (Eff)
import Effectful.Log (Logger)
import Moe.App.Effect.Runner
  ( runMetadataEffects,
    runRenameEffects,
    runSubscriptionEffects,
  )
import Moe.App.Env (MoeEnv)
import Moe.App.Job.Types (MetadataJobEffects, RenameJobEffects, SubscriptionJobEffects)
import Moe.Domain.Scheduler.Types (JobResult)
import Moe.Prelude

runMetadataJob ::
  Text ->
  Logger ->
  MoeEnv ->
  Eff MetadataJobEffects JobResult ->
  IO JobResult
runMetadataJob logPrefix logger env = runMetadataEffects env logger logPrefix

runSubscriptionJob ::
  Text ->
  Logger ->
  MoeEnv ->
  Eff SubscriptionJobEffects JobResult ->
  IO JobResult
runSubscriptionJob logPrefix logger env = runSubscriptionEffects env logger logPrefix

runRenameJob ::
  Text ->
  Logger ->
  MoeEnv ->
  Eff RenameJobEffects JobResult ->
  IO JobResult
runRenameJob logPrefix logger env = runRenameEffects env logger logPrefix
