module Moe.Core.Bangumi.Internal.Content
  ( SeasonNum (..),
    EpisodeNum (..),
    Index (..),
    Year (..),
    TmdbId (..),
    EpisodeType (..),
    ExtraContent (..),
    TrailerContent (..),
    BangumiContent (..),
  )
where


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
