-- | Handlers for collection torrent management.
module Moe.Web.API.Collection.Handler
  ( handleCollectionAdd,
    handleCollectionFiles,
    handleCollectionConfirm,
    handleCollectionCancel,
  )
where

import Moe.Infra.Downloader.Effect
  ( addTorrent,
    addTagsToTorrents,
    deleteTorrents,
    getTorrentFiles,
    setFilePriority,
    AddTorrentParams (..),
    TorrentContent (..),
    collectionTag,
    renameTag,
  )
import Moe.Prelude
import Moe.Web.API.DTO.Collection
import Moe.Web.Error (throwValidation)
import Moe.Web.Types (ServerEff)
import Servant (NoContent (..))

-- | Add a torrent paused with collection tag, return its hash.
handleCollectionAdd :: CollectionAddRequest -> ServerEff CollectionAddResponse
handleCollectionAdd req = do
  let params =
        AddTorrentParams
          { url = req.torrentUrl,
            savePath = Nothing,
            rename = Nothing,
            tags = Just [collectionTag]
          }
  addTorrent params
  hash <- case req.infoHash of
    Just h -> pure h
    Nothing -> throwValidation "infoHash is required for collection add"
  pure CollectionAddResponse {hash}

-- | Get file list for a torrent by hash.
handleCollectionFiles :: Text -> ServerEff CollectionFilesResponse
handleCollectionFiles hash = do
  files <- getTorrentFiles hash
  pure CollectionFilesResponse {files = map toFileDTO files}

-- | Convert TorrentContent to TorrentFileDTO.
toFileDTO :: TorrentContent -> TorrentFileDTO
toFileDTO tc =
  TorrentFileDTO
    { index = tc.index,
      name = tc.name,
      size = tc.size
    }

-- | Set priority 0 for unwanted files, then add rename tag.
handleCollectionConfirm :: CollectionConfirmRequest -> ServerEff NoContent
handleCollectionConfirm req = do
  unless (null req.unwantedIndices) $
    setFilePriority req.hash req.unwantedIndices 0
  addTagsToTorrents [req.hash] [renameTag]
  pure NoContent

-- | Cancel a collection torrent by deleting it.
handleCollectionCancel :: Text -> ServerEff NoContent
handleCollectionCancel hash = do
  deleteTorrents [hash] True
  pure NoContent
