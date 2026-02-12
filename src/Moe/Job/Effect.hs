-- | Base effect stack and shared helpers for job execution.
module Moe.Job.Effect
  ( -- * Effect stack types
    BaseEffects,

    -- * Effect runners
    runBaseEffects,

    -- * Helpers
    logAppError,
  )
where

import Data.Text.Display (display)
import Effectful
import Effectful.Concurrent (runConcurrent)
import Effectful.Error.Static (runErrorWith)
import Effectful.Log (Logger)
import Effectful.Log qualified as Log
import Effectful.Sqlite (SqliteDb (..), runSqlite)
import Moe.App.Env (MoeConfig (..), MoeEnv (..))
import Moe.App.Logging (LogConfig (..), runLog)
import Moe.Error (AppError (..))
import Moe.Prelude

type BaseEffects =
  '[ Error AppError,
     Log,
     Sqlite,
     Concurrent,
     IOE
   ]

runBaseEffects ::
  MoeEnv ->
  Logger ->
  Text ->
  Eff BaseEffects () ->
  IO ()
runBaseEffects env logger jobName action =
  action
    & runErrorWith (logAppError jobName)
    & runLog jobName logger env.config.logConfig.logLevel
    & runSqlite (DbPool env.dbPool)
    & runConcurrent
    & withUnliftStrategy (ConcUnlift Ephemeral Unlimited)
    & runEff

logAppError :: (Log :> es) => Text -> a -> AppError -> Eff es ()
logAppError jobName _ err =
  Log.logAttention_ $ jobName <> display err
