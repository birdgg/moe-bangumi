module Moe.Domain.Episode
  ( EpisodeId,
    EpisodeNumber (..),
    Episode (..),
  )
where

import Effectful.Sqlite (FromRow, ToRow)
import Moe.Domain.Bangumi (BangumiId)
import Moe.Domain.Rss (PubDate, TorrentUrl)
import Moe.Domain.Shared.Entity (Id)
import Moe.Domain.Shared.Numbering (EpisodeNumber (..))
import Moe.Domain.Shared.Group (GroupName (..))
import Moe.Domain.Shared.Subtitle (SubtitleList)
import Moe.Prelude

type EpisodeId = Id Episode

data Episode = Episode
  { bangumiId :: BangumiId,
    episodeNumber :: EpisodeNumber,
    group :: [GroupName],
    subtitleList :: SubtitleList,
    resolution :: Maybe Text,
    infoHash :: Text,
    torrentUrl :: TorrentUrl,
    pubDate :: PubDate
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromRow, ToRow)
