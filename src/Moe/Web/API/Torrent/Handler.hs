module Moe.Web.API.Torrent.Handler
  ( handleSearchTorrent,
    handleDownloadTorrent,
  )
where

import Moe.Infra.Rss.Types (RssFetchError)
import Moe.Infra.Downloader.Effect (addTorrent, AddTorrentParams (..), collectionTag, renameTag)
import Moe.Infra.Rss.Effect (Rss, fetchRss)
import Moe.Infra.Rss.Source (Nyaa, searchUrlFor)
import Moe.Prelude
import Moe.Web.API.DTO.Torrent (DownloadTorrentRequest (..), TorrentSearchResult, toTorrentSearchResult)
import Moe.Web.Types (ServerEff)
import Servant (NoContent (..))

-- | Search Nyaa for torrents matching the keyword.
handleSearchTorrent :: Text -> ServerEff [TorrentSearchResult]
handleSearchTorrent keyword =
  case searchUrlFor (Proxy @Nyaa) keyword of
    Nothing -> pure []
    Just url -> searchSource "Nyaa" url

-- | Fetch and convert results from a single source, returning [] on error.
searchSource :: (Rss :> es, Error RssFetchError :> es) => Text -> Text -> Eff es [TorrentSearchResult]
searchSource sourceName url = do
  items <- fetchRss url `catchError` \_ (_ :: RssFetchError) -> pure []
  pure $ mapMaybe (toTorrentSearchResult sourceName) items

-- | Download a torrent directly to the downloader with collection tag.
handleDownloadTorrent :: DownloadTorrentRequest -> ServerEff NoContent
handleDownloadTorrent req = do
  let params =
        AddTorrentParams
          { url = req.torrentUrl,
            savePath = Nothing,
            rename = Nothing,
            tags = Just [collectionTag, renameTag]
          }
  addTorrent params
  pure NoContent
