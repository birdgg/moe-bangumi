module Moe.App.Subscription.Types
  ( FetchResult (..),
    FilteredItem (..),
    DownloadTask (..),
  )
where

import Data.Text (Text)
import Data.Time (UTCTime)
import Moe.Domain.Bangumi.Types (Bangumi)
import Moe.Infrastructure.Rss.Types (RawItem)

data FetchResult = FetchResult
  { bangumi :: Bangumi,
    rssUrl :: Text,
    lastPubdate :: Maybe UTCTime,
    items :: [RawItem]
  }
  deriving stock (Show, Eq)

data FilteredItem = FilteredItem
  { bangumi :: Bangumi,
    item :: RawItem,
    parsedPubDate :: UTCTime
  }
  deriving stock (Show, Eq)

data DownloadTask = DownloadTask
  { bangumi :: Bangumi,
    torrentUrl :: Text,
    infoHash :: Maybe Text,
    pubDate :: UTCTime
  }
  deriving stock (Show, Eq)
