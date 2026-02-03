module Moe.Domain.Bangumi.File.Types
  ( SubtitleLang (..),
    SubtitleList,
    SubtitleExt (..),
    VideoExt (..),
    FileType (..),
    SeasonNumber (..),
    EpisodeIndex (..),
    Index (..),
    Year (..),
    TmdbId (..),
    EpisodeType (..),
    ExtraContent (..),
    TrailerContent (..),
    BangumiContent (..),
    BangumiMeta (..),
    BangumiFile (..),
  )
where

import Data.Text (Text)
import Data.Word (Word8, Word16)
import Moe.Domain.Bangumi.Episode.Types (EpisodeIndex (..), SeasonNumber (..))
import Moe.Domain.Bangumi.Subtitle.Types
import Moe.Domain.Bangumi.Types (TmdbId (..))
import Relude (ToText (..))

newtype Index = Index Word8
  deriving stock (Eq, Show)
  deriving newtype (Num)

newtype Year = Year Word16
  deriving stock (Eq, Show)
  deriving newtype (Num)

data EpisodeType
  = Regular SeasonNumber EpisodeIndex
  | Special EpisodeIndex
  deriving stock (Eq, Show)

data ExtraContent
  = NCOP (Maybe Index)
  | NCED (Maybe Index)
  | Menu (Maybe Index)
  deriving stock (Eq, Show)

data TrailerContent
  = PV Index
  | Preview
  | Trailer
  | CM (Maybe Index)
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
    fileType :: FileType
  }
  deriving stock (Eq, Show)
