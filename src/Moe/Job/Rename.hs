-- | Rename job: moves and renames completed torrents from tmp to save path.
--
-- A dedicated worker thread ('renameWorkerThread') polls every 15 minutes,
-- dispatching to subscription or collection strategy based on torrent tags.
module Moe.Job.Rename
  ( renameWorkerThread,
  )
where

import Moe.Job.Rename.Worker (renameWorkerThread)
