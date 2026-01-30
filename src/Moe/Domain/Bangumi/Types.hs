module Moe.Domain.Bangumi.Types
  ( BangumiId (..),
    TmdbId (..),
    BgmtvId (..),
    MikanId (..),
    BangumiSeason (..),
    Season (..),
    Bangumi (..),
    airDateToBangumiSeason,
    seasonToMonths,
  )
where

import Data.Time.Calendar (Day, Year, toGregorian)

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

data BangumiSeason = BangumiSeason
  { year :: Year,
    season :: Season
  }
  deriving stock (Eq, Show, Ord)

instance ToText BangumiSeason where
  toText (BangumiSeason y s) = show y <> " " <> toText s

airDateToBangumiSeason :: Day -> BangumiSeason
airDateToBangumiSeason day =
  let (y, m, _) = toGregorian day
   in BangumiSeason y (monthToSeason m)

monthToSeason :: Int -> Season
monthToSeason m
  | m >= 1 && m <= 3 = Winter
  | m >= 4 && m <= 6 = Spring
  | m >= 7 && m <= 9 = Summer
  | otherwise = Fall

seasonToMonths :: Season -> [Int]
seasonToMonths Winter = [1, 2, 3]
seasonToMonths Spring = [4, 5, 6]
seasonToMonths Summer = [7, 8, 9]
seasonToMonths Fall = [10, 11, 12]

data Bangumi = Bangumi
  { id :: Maybe BangumiId,
    name :: Text,
    airDate :: Maybe Day,
    mikanId :: Maybe MikanId,
    tmdbId :: Maybe TmdbId,
    bgmtvId :: Maybe BgmtvId,
    posterUrl :: Maybe Text
  }
  deriving stock (Eq, Show)
