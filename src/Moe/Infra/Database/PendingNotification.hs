-- | Database operations for pending download notifications.
module Moe.Infra.Database.PendingNotification
  ( PendingNotification (..),
    insertPendingNotification,
    getPendingNotificationsByHashes,
    deletePendingNotificationsByHashes,
  )
where

import Data.Text qualified as T
import Moe.Prelude

-- | A pending notification awaiting download completion.
data PendingNotification = PendingNotification
  { infoHash :: Text,
    title :: Text,
    posterUrl :: Maybe Text
  }
  deriving stock (Show, Eq)

instance FromRow PendingNotification where
  fromRow = PendingNotification <$> field <*> field <*> field

-- | Insert a pending notification for a torrent hash.
insertPendingNotification ::
  (SqliteTransaction :> es, IOE :> es) =>
  PendingNotification ->
  Eff es ()
insertPendingNotification pn =
  execute
    "INSERT INTO pending_notification (info_hash, title, poster_url) VALUES (?, ?, ?)"
    (pn.infoHash, pn.title, pn.posterUrl)

-- | Get all pending notifications for a list of info hashes.
getPendingNotificationsByHashes ::
  (SqliteTransaction :> es, IOE :> es) =>
  [Text] ->
  Eff es [PendingNotification]
getPendingNotificationsByHashes [] = pure []
getPendingNotificationsByHashes hashes = do
  let placeholders = T.intercalate "," $ replicate (length hashes) "?"
      sql = "SELECT info_hash, title, poster_url FROM pending_notification WHERE info_hash IN (" <> placeholders <> ")"
  query (fromString $ toString sql) hashes

-- | Delete all pending notifications for a list of info hashes.
deletePendingNotificationsByHashes ::
  (SqliteTransaction :> es, IOE :> es) =>
  [Text] ->
  Eff es ()
deletePendingNotificationsByHashes [] = pass
deletePendingNotificationsByHashes hashes = do
  let placeholders = T.intercalate "," $ replicate (length hashes) "?"
      sql = "DELETE FROM pending_notification WHERE info_hash IN (" <> placeholders <> ")"
  execute (fromString $ toString sql) hashes
