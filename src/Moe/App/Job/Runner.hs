module Moe.App.Job.Runner
  ( runMetadataJob,
    runRssJob,
  )
where

import Effectful (Eff)
import Effectful.Log (Logger)
import Moe.App.Effect.Runner (runMetadataEffects, runRssEffects)
import Moe.App.Env (MoeEnv)
import Moe.App.Job.Types (MetadataJobEffects, RssJobEffects)
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
