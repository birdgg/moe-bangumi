module Moe.Domain.Bangumi.File.Types
  ( SubtitleLang (..),
    SubtitleList,
    SubtitleExt (..),
    VideoExt (..),
    FileType (..),
    EpisodeNumber (..),
    ExtraIndex (..),
    Year,
    TmdbId (..),
    SeasonNumber (..),
    GroupName (..),
    EpisodeType (..),
    ExtraContent (..),
    TrailerContent (..),
    BangumiContent (..),
    BangumiMeta (..),
    BangumiFile (..),
  )
where

import Data.Time.Calendar (Year)
import Moe.Domain.Bangumi.Episode (EpisodeNumber (..))
import Moe.Domain.Bangumi.Internal.Group (GroupName (..))
import Moe.Domain.Bangumi.Internal.Subtitle (SubtitleExt (..), SubtitleLang (..), SubtitleList)
import Moe.Domain.Bangumi.Types (SeasonNumber (..), TmdbId (..))
import Moe.Prelude

newtype ExtraIndex = ExtraIndex Word8
  deriving stock (Eq, Show)
  deriving newtype (Num)

data EpisodeType
  = Regular SeasonNumber EpisodeNumber
  | Special EpisodeNumber
  deriving stock (Eq, Show)

data ExtraContent
  = NCOP (Maybe ExtraIndex)
  | NCED (Maybe ExtraIndex)
  | Menu (Maybe ExtraIndex)
  deriving stock (Eq, Show)

data TrailerContent
  = PV ExtraIndex
  | Preview
  | Trailer
  | CM (Maybe ExtraIndex)
  deriving stock (Eq, Show)

data BangumiContent
  = Episode EpisodeType
  | Extra ExtraContent
  | TrailerItem TrailerContent
  | Movie Year
  deriving stock (Eq, Show)

data VideoExt
  = MKV
  | MP4
  | AVI
  | WEBM
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance ToText VideoExt where
  toText MKV = "mkv"
  toText MP4 = "mp4"
  toText AVI = "avi"
  toText WEBM = "webm"

data FileType
  = Video VideoExt
  | Subtitle SubtitleLang SubtitleExt
  deriving stock (Eq, Show)

data BangumiMeta = BangumiMeta
  { name :: Text,
    year :: Maybe Year,
    tmdbId :: Maybe TmdbId
  }
  deriving stock (Eq, Show)

data BangumiFile = BangumiFile
  { meta :: BangumiMeta,
    content :: BangumiContent,
    fileType :: FileType,
    group :: [GroupName]
  }
  deriving stock (Eq, Show)
