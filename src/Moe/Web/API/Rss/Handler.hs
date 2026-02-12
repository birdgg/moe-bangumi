module Moe.Web.API.Rss.Handler
  ( handleSearchRss,
    handleDownloadTorrent,
  )
where

import Control.Exception.Safe (tryAny)
import Effectful ((:>))
import Effectful.Concurrent.Async (forConcurrently)
import Moe.Infra.Downloader.Effect (addTorrent, AddTorrentParams (..), collectionTag)
import Moe.Infra.Rss.Effect (Rss, fetchRss)
import Moe.Infra.Rss.Source (AcgRip, Nyaa, searchUrlFor)
import Moe.Prelude
import Moe.Web.API.DTO.Rss (DownloadTorrentRequest (..), RssSearchResult, toRssSearchResult)
import Moe.Web.Types (ServerEff)
import Servant (NoContent (..))

-- | Search RSS sources (Nyaa, AcgRip) concurrently and return combined results.
handleSearchRss :: Text -> ServerEff [RssSearchResult]
handleSearchRss keyword = do
  let sources = buildSources keyword
  results <- forConcurrently sources $ uncurry searchSource
  pure $ concat results

-- | Build list of (sourceName, searchUrl) for all supported sources.
buildSources :: Text -> [(Text, Text)]
buildSources keyword =
  mapMaybe
    (\(name, mUrl) -> (name,) <$> mUrl)
    [ ("Nyaa", searchUrlFor (Proxy @Nyaa) keyword),
      ("AcgRip", searchUrlFor (Proxy @AcgRip) keyword)
    ]

-- | Fetch and convert results from a single source, returning [] on error.
searchSource :: (Rss :> es, IOE :> es) => Text -> Text -> Eff es [RssSearchResult]
searchSource sourceName url = do
  result <- tryAny $ fetchRss url
  case result of
    Left _err -> pure []
    Right items -> pure $ mapMaybe (toRssSearchResult sourceName) items

-- | Download a torrent directly to the downloader with collection tag.
handleDownloadTorrent :: DownloadTorrentRequest -> ServerEff NoContent
handleDownloadTorrent req = do
  let params =
        AddTorrentParams
          { url = req.torrentUrl,
            savePath = Nothing,
            rename = Nothing,
            tags = Just [collectionTag]
          }
  addTorrent params
  pure NoContent
