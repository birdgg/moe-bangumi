module Moe.File.Types
  ( SubtitleLang (..),
    SubtitleList,
    VideoExt (..),
    SubtitleExt (..),
    FileType (..),
    SeasonNum (..),
    EpisodeNum (..),
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

import Data.Text.Display

newtype SeasonNum = SeasonNum Word8
  deriving stock (Eq, Show)
  deriving newtype (Num)

newtype EpisodeNum = EpisodeNum Word8
  deriving stock (Eq, Show)
  deriving newtype (Num)

newtype Index = Index Word8
  deriving stock (Eq, Show)
  deriving newtype (Num)

newtype Year = Year Word16
  deriving stock (Eq, Show)
  deriving newtype (Num)

newtype TmdbId = TmdbId Word32
  deriving stock (Eq, Show)
  deriving newtype (Num)

data EpisodeType
  = Regular SeasonNum EpisodeNum
  | Special EpisodeNum
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

data SubtitleLang
  = CHS
  | CHT
  | JPN
  | ENG
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance Display SubtitleLang where
  displayBuilder CHS = "简"
  displayBuilder CHT = "繁"
  displayBuilder JPN = "日"
  displayBuilder ENG = "英"

instance ToText SubtitleLang where
  toText CHS = "chs"
  toText CHT = "cht"
  toText JPN = "jpn"
  toText ENG = "eng"

type SubtitleList = [SubtitleLang]

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

data SubtitleExt
  = SRT
  | ASS
  | SSA
  | SUB
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance ToText SubtitleExt where
  toText SRT = "srt"
  toText ASS = "ass"
  toText SSA = "ssa"
  toText SUB = "sub"

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
