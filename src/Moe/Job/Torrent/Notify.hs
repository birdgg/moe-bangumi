-- | Notify process: sends notifications for torrents that have completed download.
module Moe.Job.Torrent.Notify
  ( runNotify,
  )
where

import Effectful.Log qualified as Log
import Moe.Infra.Database.PendingNotification (PendingNotification (..))
import Moe.Infra.Database.PendingNotification qualified as PendingNotificationDB
import Moe.Infra.Downloader.Effect
import Moe.Infra.Notification.Effect (Notification, sendNotification)
import Moe.Prelude

-- | Send notifications for completed torrents that have pending notifications.
runNotify ::
  (Notification :> es, Sqlite :> es, Concurrent :> es, Log :> es, IOE :> es) =>
  [TorrentInfo] ->
  Eff es ()
runNotify [] = pass
runNotify completed = do
  let hashes = map (\t -> t.hash.unInfoHash) completed
  pending <- transact $ PendingNotificationDB.getPendingNotificationsByHashes hashes
  for_ pending $ \pn -> do
    result <- try @SomeException $ sendNotification (pn.title <> " 已更新") pn.posterUrl
    case result of
      Left ex ->
        Log.logAttention_ $ "notify: notification failed - " <> toText (displayException ex)
      Right () -> pass
  let notifiedHashes = map (.infoHash) pending
  when (not $ null notifiedHashes) $
    transact $ PendingNotificationDB.deletePendingNotificationsByHashes notifiedHashes
