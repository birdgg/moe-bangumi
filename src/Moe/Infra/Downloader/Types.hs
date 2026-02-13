-- | Domain types for download/torrent management.
module Moe.Infra.Downloader.Types
  ( AddTorrentParams (..),
    DownloaderClientError (..),
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

import Data.Text.Display (Display (..))
import Moe.Domain.Rss (TorrentUrl)
import Network.QBittorrent.Types (InfoHash (..), Tag (..), TorrentFile (..), TorrentInfo (..), TorrentState (..))
import Moe.Prelude

-- | Structured downloader client errors.
data DownloaderClientError
  = DlNetworkError Text
  | DlAuthError Text
  | DlApiError Int Text
  | DlParseError Text
  | DlInvalidTorrent Text
  | DlConfigError Text
  deriving stock (Show, Eq)

instance Display DownloaderClientError where
  displayBuilder = \case
    DlNetworkError msg -> "Downloader connection failed: " <> displayBuilder msg
    DlAuthError msg -> "Downloader auth failed: " <> displayBuilder msg
    DlApiError code msg -> "Downloader API error " <> displayBuilder (show @Text code) <> ": " <> displayBuilder msg
    DlParseError msg -> "Downloader parse error: " <> displayBuilder msg
    DlInvalidTorrent msg -> "Downloader invalid torrent: " <> displayBuilder msg
    DlConfigError msg -> "Downloader: " <> displayBuilder msg

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
