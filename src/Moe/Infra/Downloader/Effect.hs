-- | Abstract Downloader effect for torrent management.
--
-- Provides a downloader-agnostic interface that can be implemented
-- by qBittorrent, Transmission, or other torrent clients.
module Moe.Infra.Downloader.Effect
  ( -- * Effect
    Downloader (..),

    -- * Operations
    testConnection,
    addTorrent,
    getTorrentsByHashes,
    getRenameTorrents,
    getTorrentFiles,
    renameTorrentFile,
    renameTorrentFolder,
    setTorrentLocation,
    startTorrents,
    stopTorrents,
    deleteTorrents,
    addTagsToTorrents,
    removeTagsFromTorrents,

    -- * Re-exports from Types
    module Moe.Infra.Downloader.Types,
  )
where

import Effectful
import Effectful.TH (makeEffect)
import Moe.Infra.Downloader.Types
import Moe.Prelude

-- | Abstract downloader effect for torrent management.
data Downloader :: Effect where
  -- | Test authentication with explicit credentials (url, username, password).
  --   Returns Right version on success, Left error message on failure.
  TestConnection :: Text -> Text -> Text -> Downloader m (Either Text Text)
  -- | Add a torrent with configurable save path, rename, and tags.
  AddTorrent :: AddTorrentParams -> Downloader m ()
  -- | Get torrents by their info hashes.
  GetTorrentsByHashes :: [Text] -> Downloader m [TorrentInfo]
  -- | Get all torrents with the Rename tag.
  GetRenameTorrents :: Downloader m [TorrentInfo]
  -- | Get files within a torrent by its hash.
  GetTorrentFiles :: Text -> Downloader m [TorrentFile]
  -- | Rename a file within a torrent (hash, oldPath, newPath).
  RenameTorrentFile :: Text -> Text -> Text -> Downloader m ()
  -- | Rename a folder within a torrent (hash, oldPath, newPath).
  RenameTorrentFolder :: Text -> Text -> Text -> Downloader m ()
  -- | Move torrents to a new location on disk.
  SetTorrentLocation :: [Text] -> Text -> Downloader m ()
  -- | Start (resume) torrents by their hashes.
  StartTorrents :: [Text] -> Downloader m ()
  -- | Stop (pause) torrents by their hashes.
  StopTorrents :: [Text] -> Downloader m ()
  -- | Delete torrents by their hashes, optionally deleting files.
  DeleteTorrents :: [Text] -> Bool -> Downloader m ()
  -- | Add tags to torrents.
  AddTagsToTorrents :: [Text] -> [Tag] -> Downloader m ()
  -- | Remove tags from torrents.
  RemoveTagsFromTorrents :: [Text] -> [Tag] -> Downloader m ()

type instance DispatchOf Downloader = 'Dynamic

makeEffect ''Downloader
