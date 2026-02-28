-- | Cleanup process: deletes seeded torrents that have reached share ratio threshold.
module Moe.Job.Torrent.Cleanup
  ( runCleanup,
  )
where

import Effectful.Log qualified as Log
import Moe.Infra.Downloader.Effect
import Moe.Prelude

-- | Delete torrents that are completed and have reached ratio >= 1.0.
-- Files are kept on disk (deleteFiles = False).
runCleanup :: (Downloader :> es, Log :> es, IOE :> es) => [TorrentInfo] -> Eff es ()
runCleanup candidates =
  for_ candidates $ \t -> do
    deleteTorrents [t.hash.unInfoHash] False
    Log.logInfo_ $ "Cleaned up torrent: " <> t.name
