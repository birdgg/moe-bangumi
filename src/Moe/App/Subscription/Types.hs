module Moe.App.Subscription.Types
  ( FetchResult (..),
    FilteredItem (..),
    ParsedItem (..),
    DownloadTask (..),
  )
where

import Data.Time (UTCTime)
import Moe.Domain.Bangumi.Episode (EpisodeNumber)
import Moe.Domain.Bangumi.Internal.Group (GroupName)
import Moe.Domain.Bangumi.Types (Bangumi, BangumiId)
import Moe.Infrastructure.Rss.Types (RawItem)
import Moe.Prelude

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

-- | A filtered item with RSS title parsed and all required fields validated
data ParsedItem = ParsedItem
  { bangumi :: Bangumi,
    bangumiId :: BangumiId,
    torrentUrl :: Text,
    infoHash :: Text,
    pubDate :: UTCTime,
    episodeNumber :: EpisodeNumber,
    group :: Maybe GroupName,
    resolution :: Maybe Text
  }
  deriving stock (Show, Eq)

data DownloadTask = DownloadTask
  { bangumi :: Bangumi,
    torrentUrl :: Text,
    infoHash :: Maybe Text,
    pubDate :: UTCTime
  }
  deriving stock (Show, Eq)
