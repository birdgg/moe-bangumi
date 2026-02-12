module Moe.Web.API.DTO.Rss
  ( RssSearchResult (..),
    toRssSearchResult,
    DownloadTorrentRequest (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToSchema)
import Data.Time (UTCTime)
import Moe.Domain.Rss (PubDate (..))
import Moe.Infra.Rss.Types (RawItem (..))
import Moe.Prelude

-- | A single RSS search result with source attribution.
data RssSearchResult = RssSearchResult
  { title :: Text,
    source :: Text,
    torrentUrl :: Maybe Text,
    infoHash :: Maybe Text,
    pubDate :: Maybe UTCTime
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, ToSchema)

-- | Request to download a torrent via the downloader.
data DownloadTorrentRequest = DownloadTorrentRequest
  { torrentUrl :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToSchema)

-- | Convert a 'RawItem' into a 'RssSearchResult', tagging it with the source name.
toRssSearchResult :: Text -> RawItem -> Maybe RssSearchResult
toRssSearchResult sourceName item =
  case item.title of
    Nothing -> Nothing
    Just t ->
      Just
        RssSearchResult
          { title = t,
            source = sourceName,
            torrentUrl = item.torrentUrl,
            infoHash = item.infoHash,
            pubDate = unPubDate <$> item.pubDate
          }
