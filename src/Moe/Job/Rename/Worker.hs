-- | Rename worker thread: periodically processes completed torrents for renaming.
-- Replaces cron-based renameJob with a long-running thread that polls on a timer.
module Moe.Job.Rename.Worker
  ( renameWorkerThread,
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
import Moe.Infra.Metadata.Effect (Metadata, runMetadataHttp)
import Moe.Infra.Notification.Adapter (runNotificationDynamic)
import Moe.Infra.Notification.Effect (Notification)
import Moe.Infra.Setting.Effect (runSetting)
import Moe.Job.Effect (runBaseEffects)
import Moe.Job.Rename.Process (runRename)
import Moe.Prelude

-- | Entry point for the rename worker thread.
renameWorkerThread :: MoeEnv -> Logger -> IO ()
renameWorkerThread env logger =
  runBaseEffects env logger "Rename" $
    runSetting env.settingEnv $
      runDownloaderQBittorrent env.downloaderEnv env.httpManager $
        runNotificationDynamic env.httpManager $
          runMetadataHttp env.httpManager renameWorkerLoop

-- | Main worker loop: runs rename immediately on startup, then every 10 seconds.
renameWorkerLoop ::
  (Downloader :> es, Metadata :> es, Notification :> es, Sqlite :> es, Concurrent :> es, Log :> es, IOE :> es) =>
  Eff es ()
renameWorkerLoop = do
  Log.logInfo_ "Rename worker started"
  safeRunRename
  void $ infinitely $ do
    timerVar <- STM.registerDelay tenSeconds
    STM.atomically $ STM.readTVar timerVar >>= STM.check
    safeRunRename

-- | Run rename with error recovery to keep the worker thread alive.
safeRunRename ::
  (Downloader :> es, Metadata :> es, Notification :> es, Sqlite :> es, Concurrent :> es, Log :> es, IOE :> es) =>
  Eff es ()
safeRunRename = do
  result <- try @SomeException runRename
  case result of
    Left ex
      | Just (SomeAsyncException _) <- fromException ex -> throwIO ex
      | otherwise -> Log.logAttention_ $ "Rename worker error: " <> toText (displayException ex)
    Right () -> pass

-- | Timer interval: 10 seconds in microseconds.
tenSeconds :: Int
tenSeconds = 10 * 1_000_000
