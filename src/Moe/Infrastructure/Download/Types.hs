-- | Domain types for download/torrent management.
module Moe.Infrastructure.Download.Types
  ( TorrentUrl,
    AddTorrentParams (..),
    isCompleted,

    -- * Tags
    moeTag,
    renameTag,
    subscriptionTag,
    collectionTag,
    deletionTag,

    -- * Re-exports from qbittorrent
    Tag (..),
    TorrentInfo (..),
    TorrentFile (..),
    TorrentState (..),
    InfoHash (..),
  )
where

import Network.QBittorrent.Types (InfoHash (..), Tag (..), TorrentFile (..), TorrentInfo (..), TorrentState (..))
import Moe.Prelude

-- | URL for a torrent resource
type TorrentUrl = Text

-- | Parameters for adding a torrent.
data AddTorrentParams = AddTorrentParams
  { url :: TorrentUrl,
    savePath :: Maybe Text,
    rename :: Maybe Text,
    tags :: Maybe [Tag]
  }
  deriving stock (Eq, Show)

-- | Base tag applied to all managed torrents.
moeTag :: Tag
moeTag = "moe"

-- | Tag for torrents pending rename.
renameTag :: Tag
renameTag = "rename"

-- | Tag for torrents added via RSS subscription.
subscriptionTag :: Tag
subscriptionTag = "subscription"

-- | Tag for collected torrents.
collectionTag :: Tag
collectionTag = "collection"

-- | Tag for torrents marked for deletion.
deletionTag :: Tag
deletionTag = "deletion"

-- | Check if torrent download is completed.
isCompleted :: TorrentInfo -> Bool
isCompleted t = t.state `elem` completedStates
  where
    completedStates =
      [ Uploading,
        StalledUP,
        PausedUP,
        ForcedUP,
        QueuedUP,
        CheckingUP
      ]
