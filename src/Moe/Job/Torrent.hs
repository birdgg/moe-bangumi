-- | Torrent job: unified worker for rename, notify, and cleanup.
module Moe.Job.Torrent
  ( torrentWorkerThread,
  )
where

import Moe.Job.Torrent.Worker (torrentWorkerThread)
