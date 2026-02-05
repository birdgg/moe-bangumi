-- | Abstract Download effect for torrent management.
--
-- Provides a downloader-agnostic interface that can be implemented
-- by qBittorrent, Transmission, or other torrent clients.
module Moe.Infrastructure.Download.Effect
  ( -- * Effect
    Download (..),

    -- * Operations
    addTorrent,
    getTorrentsByHashes,
    getTorrentsWithTag,
    stopTorrents,
    deleteTorrents,
    addTagsToTorrents,
    removeTagsFromTorrents,

    -- * Error
    DownloadError (..),

    -- * Re-exports from Types
    module Moe.Infrastructure.Download.Types,
  )
where

import Effectful
import Effectful.TH (makeEffect)
import Moe.Infrastructure.Download.Types
import Moe.Prelude

-- | Download error types.
data DownloadError
  = ConnectionFailed Text
  | AuthenticationFailed Text
  | OperationFailed Text
  deriving stock (Eq, Show)

-- | Abstract download effect for torrent management.
data Download :: Effect where
  -- | Add a torrent with optional save path and tags.
  AddTorrent :: TorrentUrl -> Maybe Text -> Maybe TagList -> Download m ()
  -- | Get torrents by their info hashes.
  GetTorrentsByHashes :: [Text] -> Download m [TorrentInfo]
  -- | Get all torrents with a specific tag.
  GetTorrentsWithTag :: Tag -> Download m [TorrentInfo]
  -- | Stop (pause) torrents by their hashes.
  StopTorrents :: [Text] -> Download m ()
  -- | Delete torrents by their hashes, optionally deleting files.
  DeleteTorrents :: [Text] -> Bool -> Download m ()
  -- | Add tags to torrents.
  AddTagsToTorrents :: [Text] -> TagList -> Download m ()
  -- | Remove tags from torrents.
  RemoveTagsFromTorrents :: [Text] -> TagList -> Download m ()

type instance DispatchOf Download = 'Dynamic

makeEffect ''Download
