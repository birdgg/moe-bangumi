-- | Torrent worker thread: unified worker for rename, notify, and cleanup.
module Moe.Job.Torrent.Worker
  ( torrentWorkerThread,
  )
where

import Data.Text.Display (display)
import Effectful.Log qualified as Log
import Moe.App.Env (MoeEnv (..))
import Moe.Infra.Downloader.Adapter (runDownloaderQBittorrent)
import Moe.Infra.Media.Adapter (runMediaDynamic)
import Moe.Infra.Metadata.Effect (runMetadataHttp)
import Moe.Infra.Notification.Adapter (runNotificationDynamic)
import Moe.Infra.Setting.Effect (runSetting)
import Moe.Job.Effect (periodicWorker, runBaseEffects)
import Moe.Job.Torrent.Process (runTorrentJob)
import Moe.Prelude

-- | Entry point for the unified torrent worker thread.
torrentWorkerThread :: MoeEnv -> Logger -> IO ()
torrentWorkerThread env logger =
  runBaseEffects env logger "Torrent" $
    runSetting env.settingEnv $
      runErrorWith (\_ err -> Log.logAttention_ $ display err) $
        runDownloaderQBittorrent env.downloaderEnv $
          runNotificationDynamic $
            runMediaDynamic $
              runErrorWith (\_ err -> Log.logAttention_ $ display err) $
                runMetadataHttp $
                  periodicWorker "Torrent" (5 * 60 * 1_000_000) runTorrentJob
