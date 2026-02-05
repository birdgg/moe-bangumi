-- | Domain types for download/torrent management.
module Moe.Infrastructure.Download.Types
  ( TorrentUrl,
    AddTorrentParams (..),
    infoHashToText,
    isDownloading,
    isCompleted,

    -- * Tags
    moeTag,
    renameTag,
    subscriptionTag,
    collectionTag,
    deletionTag,

    -- * Re-exports from effectful-qbittorrent
    Tag (..),
    TorrentInfo (..),
    TorrentState (..),
    InfoHash (..),
  )
where

import Effectful.QBittorrent (InfoHash (..), Tag (..), TorrentInfo (..), TorrentState (..))
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

-- | Extract Text from InfoHash.
infoHashToText :: InfoHash -> Text
infoHashToText = (.unInfoHash)

-- | Check if torrent is currently downloading.
isDownloading :: TorrentInfo -> Bool
isDownloading t = t.state `elem` downloadingStates
  where
    downloadingStates =
      [ Downloading,
        StalledDL,
        MetaDL,
        ForcedDL,
        QueuedDL,
        CheckingDL,
        Allocating
      ]

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
