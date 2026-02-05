module Moe.Domain.Bangumi.Episode
  ( EpisodeId (..),
    EpisodeNumber (..),
    GroupName (..),
    Episode (..),
  )
where

import Data.Time (UTCTime)
import Moe.Domain.Bangumi.Internal.Episode (EpisodeNumber (..))
import Moe.Domain.Bangumi.Internal.Group (GroupName (..))
import Moe.Domain.Bangumi.Internal.Subtitle (SubtitleList)
import Moe.Domain.Bangumi.Types (BangumiId)
import Moe.Domain.Rss.Types (PubDate)
import Moe.Infrastructure.Download.Types (TorrentUrl)
import Moe.Prelude

newtype EpisodeId = EpisodeId Int64
  deriving stock (Eq, Show)

data Episode = Episode
  { id :: Maybe EpisodeId,
    bangumiId :: BangumiId,
    episodeNumber :: EpisodeNumber,
    group :: [GroupName],
    subtitleList :: SubtitleList,
    resolution :: Maybe Text,
    infoHash :: Text,
    torrentUrl :: TorrentUrl,
    pubDate :: PubDate,
    createdAt :: Maybe UTCTime
  }
  deriving stock (Eq, Show)
