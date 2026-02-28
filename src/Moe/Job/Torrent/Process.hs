-- | Unified torrent job: fetches all moe-tagged torrents once
-- and dispatches to rename, notify, and cleanup.
module Moe.Job.Torrent.Process
  ( runTorrentJob,
  )
where

import Moe.Infra.Database.Types (DatabaseExecError)
import Moe.Infra.Downloader.Effect
import Moe.Infra.Media.Effect (Media)
import Moe.Infra.Metadata.Effect (Metadata)
import Moe.Infra.Notification.Effect (Notification)
import Moe.Job.Torrent.Cleanup (runCleanup)
import Moe.Job.Torrent.Notify (runNotify)
import Moe.Job.Torrent.Rename (runRename)
import Moe.Prelude

-- | Run all torrent management jobs with a single fetch.
runTorrentJob ::
  (Downloader :> es, Metadata :> es, Notification :> es, Media :> es, Sqlite :> es, Error DatabaseExecError :> es, Concurrent :> es, Log :> es, IOE :> es) =>
  Eff es ()
runTorrentJob = do
  torrents <- getMoeTorrents
  let toRename = filter (\t -> renameTag `elem` t.tags) torrents
      completed = filter (\t -> isCompleted t && renameTag `notElem` t.tags) torrents
      toCleanup = filter (\t -> isCompleted t && t.ratio >= 1.0 && renameTag `notElem` t.tags) torrents
  runRename toRename
  runNotify completed
  runCleanup toCleanup
