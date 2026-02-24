-- | Cleanup process: deletes seeded torrents that have reached share ratio threshold.
module Moe.Job.Cleanup.Process
  ( runCleanup,
  )
where

import Effectful
import Effectful.Log qualified as Log
import Moe.Infra.Downloader.Effect
import Moe.Prelude

-- | Delete moe-tagged torrents that are completed and have reached ratio >= 1.0.
-- Files are kept on disk (deleteFiles = False).
-- Torrents still pending rename are excluded to avoid racing with the rename worker.
runCleanup :: (Downloader :> es, Log :> es, IOE :> es) => Eff es ()
runCleanup = do
  torrents <- getMoeTorrents
  let candidates = filter shouldCleanup torrents
  for_ candidates $ \t -> do
    deleteTorrents [t.hash.unInfoHash] False
    Log.logInfo_ $ "Cleaned up torrent: " <> t.name

-- | A torrent should be cleaned up when it is completed, has seeded to ratio >= 1.0,
-- and is no longer pending rename.
shouldCleanup :: TorrentInfo -> Bool
shouldCleanup t =
  isCompleted t
    && t.ratio >= 1.0
    && renameTag `notElem` t.tags
