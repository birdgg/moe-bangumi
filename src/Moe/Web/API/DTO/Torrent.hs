module Moe.Web.API.DTO.Torrent
  ( TorrentSearchResult (..),
    toTorrentSearchResult,
    DownloadTorrentRequest (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Time (UTCTime)
import Moe.Domain.Rss (PubDate (..))
import Moe.Infra.Rss.Types (RawItem (..))
import Moe.Prelude

-- | A single torrent search result with source attribution.
data TorrentSearchResult = TorrentSearchResult
  { title :: Text,
    source :: Text,
    torrentUrl :: Maybe Text,
    infoHash :: Maybe Text,
    pubDate :: Maybe UTCTime
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

-- | Request to download a torrent via the downloader.
data DownloadTorrentRequest = DownloadTorrentRequest
  { torrentUrl :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

-- | Convert a 'RawItem' into a 'TorrentSearchResult', tagging it with the source name.
toTorrentSearchResult :: Text -> RawItem -> Maybe TorrentSearchResult
toTorrentSearchResult sourceName item =
  case item.title of
    Nothing -> Nothing
    Just t ->
      Just
        TorrentSearchResult
          { title = t,
            source = sourceName,
            torrentUrl = item.torrentUrl,
            infoHash = item.infoHash,
            pubDate = unPubDate <$> item.pubDate
          }
