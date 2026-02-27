-- | Cleanup worker thread: periodically removes seeded torrents.
module Moe.Job.Cleanup.Worker
  ( cleanupWorkerThread,
  )
where

import Data.Text.Display (display)
import Effectful.Log qualified as Log
import Moe.App.Env (MoeEnv (..))
import Moe.Infra.Downloader.Adapter (runDownloaderQBittorrent)
import Moe.Infra.Setting.Effect (runSetting)
import Moe.Job.Cleanup.Process (runCleanup)
import Moe.Job.Effect (periodicWorker, runBaseEffects)
import Moe.Prelude

-- | Entry point for the cleanup worker thread.
cleanupWorkerThread :: MoeEnv -> Logger -> IO ()
cleanupWorkerThread env logger =
  runBaseEffects env logger "Cleanup" $
    runSetting env.settingEnv $
      runErrorWith (\_ err -> Log.logAttention_ $ display err) $
        runDownloaderQBittorrent env.downloaderEnv env.httpManager $
          periodicWorker "Cleanup" (5 * 60 * 1_000_000) runCleanup
