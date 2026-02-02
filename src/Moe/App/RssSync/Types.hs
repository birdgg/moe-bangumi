module Moe.App.RssSync.Types
  ( FetchResult (..),
    FilteredItem (..),
    DownloadTask (..),
    PipelineResult (..),
  )
where

import Data.Text (Text)
import Data.Time (UTCTime)
import Moe.Domain.Bangumi.Types (BangumiId)
import Moe.Infrastructure.Rss.Types (RawItem)

data FetchResult = FetchResult
  { bangumiId :: BangumiId,
    rssUrl :: Text,
    lastPubdate :: Maybe UTCTime,
    items :: [RawItem]
  }
  deriving stock (Show, Eq)

data FilteredItem = FilteredItem
  { bangumiId :: BangumiId,
    item :: RawItem,
    parsedPubDate :: UTCTime
  }
  deriving stock (Show, Eq)

data DownloadTask = DownloadTask
  { bangumiId :: BangumiId,
    torrentUrl :: Text,
    infoHash :: Maybe Text,
    pubDate :: UTCTime
  }
  deriving stock (Show, Eq)

data PipelineResult = PipelineResult
  { fetchedCount :: Int,
    filteredCount :: Int,
    downloadedCount :: Int
  }
  deriving stock (Show, Eq)
