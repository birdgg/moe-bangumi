-- | Rename job: renames paused torrents and starts download.
--
-- Dispatches to subscription or collection strategy based on torrent tags.
module Moe.Job.Rename.Process
  ( runRename,
  )
where

import Control.Exception (SomeAsyncException (..))
import Effectful
import Effectful.Exception (throwIO, try)
import Effectful.Log qualified as Log
import Moe.Infra.Downloader.Effect
import Moe.Infra.Metadata.Effect (Metadata)
import Moe.Infra.Notification.Effect (Notification)
import Moe.Job.Rename.Strategy.Collection (renameCollection)
import Moe.Job.Rename.Strategy.Subscription (renameSubscription)
import Moe.Prelude

-- | Run the rename process for all torrents with rename tag.
runRename ::
  (Downloader :> es, Metadata :> es, Notification :> es, Sqlite :> es, Concurrent :> es, Log :> es, IOE :> es) =>
  Eff es ()
runRename = do
  torrents <- getRenameTorrents
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
