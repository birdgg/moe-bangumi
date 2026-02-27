-- | Base effect stack and shared helpers for job execution.
module Moe.Job.Effect
  ( -- * Effect stack types
    BaseEffects,

    -- * Effect runners
    runBaseEffects,

    -- * Worker combinators
    periodicWorker,
  )
where

import Control.Exception (SomeAsyncException (..))
import Data.Text.Display (display)
import Effectful.Concurrent.STM qualified as STM
import Effectful.Log qualified as Log
import Moe.App.Env (MoeConfig (..), MoeEnv (..))
import Moe.App.Logging (LogConfig (..), runLog)
import Moe.Infra.Database.Types (DatabaseExecError)
import Moe.Prelude

type BaseEffects =
  '[ Error DatabaseExecError,
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
    & runErrorWith (\_ err -> Log.logAttention_ $ display err)
    & runLog jobName logger env.config.logConfig.logLevel
    & runSqlite (DbPool env.dbPool)
    & runConcurrent
    & withUnliftStrategy (ConcUnlift Ephemeral Unlimited)
    & runEff

-- | Periodic worker: run action once immediately, then repeat on a fixed interval.
-- Catches and logs exceptions (re-throws async exceptions for proper shutdown).
periodicWorker :: (Concurrent :> es, Log :> es, IOE :> es) => Text -> Int -> Eff es () -> Eff es ()
periodicWorker name interval action = do
  safeRun action
  void $ infinitely $ do
    timerVar <- STM.registerDelay interval
    STM.atomically $ STM.readTVar timerVar >>= STM.check
    safeRun action
 where
  safeRun act = do
    result <- try @SomeException act
    case result of
      Left ex
        | Just (SomeAsyncException _) <- fromException ex -> throwIO ex
        | otherwise -> Log.logAttention_ $ name <> " worker error: " <> toText (displayException ex)
      Right () -> pass
