-- | Domain types for download/torrent management.
module Moe.Infrastructure.Download.Types
  ( TorrentUrl,
    MoeTag (..),
    MoeTagList (..),
    AddTorrentParams (..),
    fromTagText,
    infoHashToText,
    isDownloading,
    isCompleted,

    -- * Re-exports from effectful-qbittorrent
    TorrentInfo (..),
    TorrentState (..),
    InfoHash (..),
  )
where

import Effectful.QBittorrent (InfoHash (..), Tag (..), TorrentInfo (..), TorrentState (..), tagsToText)
import Moe.Prelude

-- | URL for a torrent resource
type TorrentUrl = Text

-- | Parameters for adding a torrent.
data AddTorrentParams = AddTorrentParams
  { url :: TorrentUrl,
    savePath :: Maybe Text,
    rename :: Maybe Text,
    tags :: Maybe MoeTagList
  }
  deriving stock (Eq, Show)

-- | Tags used for torrent management
data MoeTag =  Rename | Subscription | Collection | Deletion
  deriving stock (Eq, Ord, Show, Bounded, Enum)

instance ToText MoeTag where
  toText Rename = "rename"
  toText Subscription = "subscription"
  toText Collection = "collection"
  toText Deletion = "deletion"

fromTagText :: Text -> Maybe MoeTag
fromTagText = inverseMap toText

-- | Extract Text from InfoHash
infoHashToText :: InfoHash -> Text
infoHashToText = (.unInfoHash)

newtype MoeTagList = MoeTagList [MoeTag]
  deriving stock (Eq, Show)
  deriving newtype (Semigroup, Monoid)

instance ToText MoeTagList where
  toText (MoeTagList ts) = tagsToText $ map (Tag . toText) ts

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
