-- | Cleanup worker thread: periodically removes seeded torrents.
module Moe.Job.Cleanup.Worker
  ( cleanupWorkerThread,
  )
where

import Control.Exception (SomeAsyncException (..))
import Effectful
import Effectful.Concurrent.STM qualified as STM
import Effectful.Exception (throwIO, try)
import Effectful.Log (Logger)
import Effectful.Log qualified as Log
import Moe.App.Env (MoeEnv (..))
import Moe.Infra.Downloader.Adapter (runDownloaderQBittorrent)
import Moe.Infra.Downloader.Effect (Downloader)
import Moe.Infra.Setting.Effect (runSetting)
import Moe.Job.Cleanup.Process (runCleanup)
import Moe.Job.Effect (runBaseEffects)
import Moe.Prelude

-- | Entry point for the cleanup worker thread.
cleanupWorkerThread :: MoeEnv -> Logger -> IO ()
cleanupWorkerThread env logger =
  runBaseEffects env logger "Cleanup" $
    runSetting env.settingEnv $
      runDownloaderQBittorrent env.downloaderEnv env.httpManager
        cleanupWorkerLoop

-- | Main worker loop: runs cleanup on startup, then every 5 minutes.
cleanupWorkerLoop ::
  (Downloader :> es, Concurrent :> es, Log :> es, IOE :> es) =>
  Eff es ()
cleanupWorkerLoop = do
  Log.logInfo_ "Cleanup worker started"
  safeRunCleanup
  void $ infinitely $ do
    timerVar <- STM.registerDelay fiveMinutes
    STM.atomically $ STM.readTVar timerVar >>= STM.check
    safeRunCleanup

-- | Run cleanup with error recovery to keep the worker thread alive.
safeRunCleanup ::
  (Downloader :> es, Concurrent :> es, Log :> es, IOE :> es) =>
  Eff es ()
safeRunCleanup = do
  result <- try @SomeException runCleanup
  case result of
    Left ex
      | Just (SomeAsyncException _) <- fromException ex -> throwIO ex
      | otherwise -> Log.logAttention_ $ "Cleanup worker error: " <> toText (displayException ex)
    Right () -> pass

-- | Timer interval: 5 minutes in microseconds.
fiveMinutes :: Int
fiveMinutes = 5 * 60 * 1_000_000
