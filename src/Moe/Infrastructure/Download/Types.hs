-- | Domain types for download/torrent management.
module Moe.Infrastructure.Download.Types
  ( TorrentUrl,
    Tag (..),
    TagList (..),
    fromTagText,
    isDownloading,
    isCompleted,

    -- * Re-exports from effectful-qbittorrent
    TorrentInfo (..),
    TorrentState (..),
  )
where

import Data.Text qualified as T
import Effectful.QBittorrent (TorrentInfo (..), TorrentState (..))
import Moe.Prelude

-- | URL for a torrent resource
type TorrentUrl = Text

-- | Tags used for torrent management
data Tag = Moe | Rename | Subscription | Collection | Deletion
  deriving stock (Eq, Ord, Show, Bounded, Enum)

instance ToText Tag where
  toText Moe = "moe"
  toText Rename = "rename"
  toText Subscription = "subscription"
  toText Collection = "collection"
  toText Deletion = "deletion"

fromTagText :: Text -> Maybe Tag
fromTagText = inverseMap toText

newtype TagList = TagList [Tag]
  deriving stock (Eq, Show)

instance ToText TagList where
  toText (TagList ts) = T.intercalate "," $ map toText ts

-- | Check if torrent is currently downloading
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

-- | Check if torrent download is completed
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
