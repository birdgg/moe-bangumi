-- | Handlers for download manager endpoints.
module Moe.Web.API.Download.Handler
  ( handleListDownloads,
    handlePauseDownloads,
    handleResumeDownloads,
    handleDeleteDownloads,
  )
where

import Moe.Infra.Downloader.Effect (deleteTorrents, getMoeTorrents, startTorrents, stopTorrents)
import Moe.Prelude
import Moe.Web.API.DTO.Download (DownloadItem, TorrentActionRequest (..), toDownloadItem)
import Moe.Web.Types (ServerEff)
import Servant (NoContent (..))

-- | List all moe-tagged torrents.
handleListDownloads :: ServerEff [DownloadItem]
handleListDownloads = map toDownloadItem <$> getMoeTorrents

-- | Pause torrents by hashes.
handlePauseDownloads :: TorrentActionRequest -> ServerEff NoContent
handlePauseDownloads req = do
  stopTorrents req.hashes
  pure NoContent

-- | Resume torrents by hashes.
handleResumeDownloads :: TorrentActionRequest -> ServerEff NoContent
handleResumeDownloads req = do
  startTorrents req.hashes
  pure NoContent

-- | Delete torrents by hashes, optionally deleting files.
handleDeleteDownloads :: TorrentActionRequest -> ServerEff NoContent
handleDeleteDownloads req = do
  deleteTorrents req.hashes (fromMaybe False req.deleteFiles)
  pure NoContent
