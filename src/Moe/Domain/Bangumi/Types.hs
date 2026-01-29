module Moe.Domain.Bangumi.Types
  ( BangumiId (..),
    TmdbId (..),
    BgmtvId (..),
    MikanId (..),
    AnimeSeason (..),
    Season (..),
    Bangumi (..),
    animeSeasonFromMonth,
    animeSeasonToText,
    seasonToMonths,
  )
where

newtype BangumiId = BangumiId Int64
  deriving stock (Eq, Show)

newtype TmdbId = TmdbId Word32
  deriving stock (Eq, Show)
  deriving newtype (Num)

newtype BgmtvId = BgmtvId Word32
  deriving stock (Eq, Show)
  deriving newtype (Num)

newtype MikanId = MikanId Word32
  deriving stock (Eq, Show)
  deriving newtype (Num)

data Season = Winter | Spring | Summer | Fall
  deriving stock (Eq, Show, Ord, Enum, Bounded)

instance ToText Season where
  toText Winter = "Winter"
  toText Spring = "Spring"
  toText Summer = "Summer"
  toText Fall = "Fall"

data AnimeSeason = AnimeSeason
  { year :: Word16,
    season :: Season
  }
  deriving stock (Eq, Show, Ord)

instance ToText AnimeSeason where
  toText (AnimeSeason y s) = show y <> " " <> toText s

animeSeasonFromMonth :: Word16 -> Int -> AnimeSeason
animeSeasonFromMonth y month =
  AnimeSeason y (monthToSeason month)

monthToSeason :: Int -> Season
monthToSeason m
  | m >= 1 && m <= 3 = Winter
  | m >= 4 && m <= 6 = Spring
  | m >= 7 && m <= 9 = Summer
  | otherwise = Fall

animeSeasonToText :: AnimeSeason -> Text
animeSeasonToText = toText

seasonToMonths :: Season -> [Int]
seasonToMonths Winter = [1, 2, 3]
seasonToMonths Spring = [4, 5, 6]
seasonToMonths Summer = [7, 8, 9]
seasonToMonths Fall = [10, 11, 12]

data Bangumi = Bangumi
  { id :: Maybe BangumiId,
    name :: Text,
    year :: Maybe Word16,
    animeSeason :: Maybe AnimeSeason,
    mikanId :: Maybe MikanId,
    tmdbId :: Maybe TmdbId,
    bgmtvId :: Maybe BgmtvId,
    posterUrl :: Maybe Text,
    overview :: Maybe Text
  }
  deriving stock (Eq, Show)
