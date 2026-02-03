module Moe.Domain.Bangumi.Episode.Types
  ( EpisodeId (..),
    EpisodeNumber (..),
    Episode (..),
  )
where

import Data.Int (Int64)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Word (Word32)
import Moe.Domain.Bangumi.Types (BangumiId)

newtype EpisodeId = EpisodeId Int64
  deriving stock (Eq, Show)

newtype EpisodeNumber = EpisodeNumber Word32
  deriving stock (Eq, Ord, Show)
  deriving newtype (Num)

data Episode = Episode
  { id :: Maybe EpisodeId,
    bangumiId :: BangumiId,
    episodeNumber :: EpisodeNumber,
    subtitleGroup :: Maybe Text,
    resolution :: Maybe Text,
    infoHash :: Text,
    torrentUrl :: Text,
    pubDate :: UTCTime,
    createdAt :: Maybe UTCTime
  }
  deriving stock (Eq, Show)
