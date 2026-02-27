-- | Rename worker thread: periodically processes completed torrents for renaming.
module Moe.Job.Rename.Worker
  ( renameWorkerThread,
  )
where

import Data.Text.Display (display)
import Effectful.Log qualified as Log
import Moe.App.Env (MoeEnv (..))
import Moe.Infra.Downloader.Adapter (runDownloaderQBittorrent)
import Moe.Infra.Metadata.Effect (runMetadataHttp)
import Moe.Infra.Notification.Adapter (runNotificationDynamic)
import Moe.Infra.Setting.Effect (runSetting)
import Moe.Job.Effect (periodicWorker, runBaseEffects)
import Moe.Job.Rename.Process (runRename)
import Moe.Prelude

-- | Entry point for the rename worker thread.
renameWorkerThread :: MoeEnv -> Logger -> IO ()
renameWorkerThread env logger =
  runBaseEffects env logger "Rename" $
    runSetting env.settingEnv $
      runErrorWith (\_ err -> Log.logAttention_ $ display err) $
        runDownloaderQBittorrent env.downloaderEnv env.httpManager $
          runNotificationDynamic env.httpManager $
            runErrorWith (\_ err -> Log.logAttention_ $ display err) $
              runMetadataHttp env.httpManager $
                periodicWorker "Rename" (10 * 1_000_000) runRename
