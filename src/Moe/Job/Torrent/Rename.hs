-- | Rename job: renames paused torrents and starts download.
--
-- Dispatches to subscription or collection strategy based on torrent tags.
module Moe.Job.Torrent.Rename
  ( runRename,
  )
where

import Control.Exception (SomeAsyncException (..))
import Effectful.Log qualified as Log
import Moe.Infra.Database.Types (DatabaseExecError)
import Moe.Infra.Downloader.Effect
import Moe.Infra.Metadata.Effect (Metadata)
import Moe.Job.Torrent.Rename.Collection (renameCollection)
import Moe.Job.Torrent.Rename.Subscription (renameSubscription)
import Moe.Prelude

-- | Run the rename process for a list of torrents with rename tag.
runRename ::
  (Downloader :> es, Metadata :> es, Sqlite :> es, Error DatabaseExecError :> es, Concurrent :> es, Log :> es, IOE :> es) =>
  [TorrentInfo] ->
  Eff es ()
runRename torrents =
  forM_ torrents $ \torrent -> do
    let hash = torrent.hash.unInfoHash
        tags = torrent.tags
    result <- try @SomeException $ do
      if subscriptionTag `elem` tags
        then renameSubscription torrent hash
        else when (collectionTag `elem` tags) $
          renameCollection torrent hash
    case result of
      Left ex
        | Just (SomeAsyncException _) <- fromException ex -> throwIO ex
        | otherwise ->
            Log.logAttention_ $
              "rename: failed for " <> torrent.name <> " - " <> toText (displayException ex)
      Right () -> pass
