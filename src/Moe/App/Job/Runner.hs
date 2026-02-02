module Moe.App.Job.Runner
  ( runMetadataJob,
    runRssJob,
  )
where

import Data.Text (Text)
import Data.Text qualified as T
import Effectful
import Effectful.Concurrent (runConcurrent)
import Effectful.Error.Static (runErrorWith, throwError)
import Effectful.Log (Log, Logger)
import Effectful.Log qualified as Log
import Effectful.Sqlite (SqliteDb (..), runSqlite)
import Moe.App.Env (MoeConfig (..), MoeEnv (..), getSettingPath)
import Moe.App.Job.Types
import Moe.App.Logging (LogConfig (..), runLog)
import Moe.Domain.Scheduler.Types (JobResult (..))
import Moe.Effect.BangumiData (runBangumiDataHttp)
import Moe.Effect.Metadata (runMetadataHttp)
import Moe.Effect.Rss (runRss)
import Moe.Effect.Setting (runSettingTVar)
import Moe.Error (MoeError (..))

runMetadataJob ::
  Text ->
  Logger ->
  MoeEnv ->
  Eff MetadataJobEffects JobResult ->
  IO JobResult
runMetadataJob logPrefix logger env action =
  runEff $
    withUnliftStrategy (ConcUnlift Ephemeral Unlimited) $
      runConcurrent $
        runSqlite (DbPool env.dbPool) $
          runLog logPrefix logger env.config.logConfig.logLevel $
            runSettingTVar env.settingVar (getSettingPath env) $
              runErrorWith logMoeError $
                runErrorWith (\_ (err :: Text) -> throwError $ toMoeError err) $
                  runBangumiDataHttp env.httpManager $
                    runMetadataHttp env.httpManager action
  where
    logMoeError :: (Log :> es) => a -> MoeError -> Eff es JobResult
    logMoeError _ err = do
      Log.logAttention_ $ logPrefix <> " error: " <> T.pack (show err)
      pure $ JobFailure $ T.pack (show err)

    toMoeError :: Text -> MoeError
    toMoeError = ExternalApiError

runRssJob ::
  Text ->
  Logger ->
  MoeEnv ->
  Eff RssJobEffects JobResult ->
  IO JobResult
runRssJob logPrefix logger env action =
  runEff $
    withUnliftStrategy (ConcUnlift Ephemeral Unlimited) $
      runConcurrent $
        runSqlite (DbPool env.dbPool) $
          runLog logPrefix logger env.config.logConfig.logLevel $
            runErrorWith logMoeError $
              runRss env.httpManager action
  where
    logMoeError :: (Log :> es) => a -> MoeError -> Eff es JobResult
    logMoeError _ err = do
      Log.logAttention_ $ logPrefix <> " error: " <> T.pack (show err)
      pure $ JobFailure $ T.pack (show err)
