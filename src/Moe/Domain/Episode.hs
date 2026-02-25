module Moe.Domain.Episode
  ( EpisodeId,
    EpisodeIndex (..),
    Episode (..),
  )
where

import Moe.Domain.Bangumi (BangumiId)
import Moe.Domain.Rss (PubDate, TorrentUrl)
import Moe.Domain.Shared.Entity (Id)
import Moe.Domain.Shared.Numbering (EpisodeIndex (..))
import Moe.Domain.Shared.Group (GroupName (..))
import Moe.Domain.Shared.Subtitle (SubtitleList)
import Moe.Prelude

type EpisodeId = Id Episode

data Episode = Episode
  { bangumiId :: BangumiId,
    episodeNumber :: EpisodeIndex,
    group :: [GroupName],
    subtitleList :: SubtitleList,
    resolution :: Maybe Text,
    infoHash :: Text,
    torrentUrl :: TorrentUrl,
    pubDate :: PubDate
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromRow, ToRow)
