-- | Cleanup job: auto-deletes seeded torrents that have reached share ratio threshold.
module Moe.Job.Cleanup
  ( cleanupWorkerThread,
  )
where

import Moe.Job.Cleanup.Worker (cleanupWorkerThread)
