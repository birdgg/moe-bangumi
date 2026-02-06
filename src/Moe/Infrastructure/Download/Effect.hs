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
    getRenameTorrents,
    getTorrentFiles,
    renameTorrentFile,
    setTorrentLocation,
    stopTorrents,
    deleteTorrents,
    addTagsToTorrents,
    removeTagsFromTorrents,

    -- * Re-exports from Types
    module Moe.Infrastructure.Download.Types,
  )
where

import Effectful
import Effectful.TH (makeEffect)
import Moe.Infrastructure.Download.Types
import Moe.Prelude

-- | Abstract download effect for torrent management.
data Download :: Effect where
  -- | Add a torrent with configurable save path, rename, and tags.
  AddTorrent :: AddTorrentParams -> Download m ()
  -- | Get torrents by their info hashes.
  GetTorrentsByHashes :: [Text] -> Download m [TorrentInfo]
  -- | Get all torrents with the Rename tag.
  GetRenameTorrents :: Download m [TorrentInfo]
  -- | Get files within a torrent by its hash.
  GetTorrentFiles :: Text -> Download m [TorrentFile]
  -- | Rename a file within a torrent (hash, oldPath, newPath).
  RenameTorrentFile :: Text -> Text -> Text -> Download m ()
  -- | Move torrents to a new location on disk.
  SetTorrentLocation :: [Text] -> Text -> Download m ()
  -- | Stop (pause) torrents by their hashes.
  StopTorrents :: [Text] -> Download m ()
  -- | Delete torrents by their hashes, optionally deleting files.
  DeleteTorrents :: [Text] -> Bool -> Download m ()
  -- | Add tags to torrents.
  AddTagsToTorrents :: [Text] -> [Tag] -> Download m ()
  -- | Remove tags from torrents.
  RemoveTagsFromTorrents :: [Text] -> [Tag] -> Download m ()

type instance DispatchOf Download = 'Dynamic

makeEffect ''Download
