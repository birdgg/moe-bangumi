-- |
-- Module      : Moe.Domain.Bangumi.Season.Types
-- Description : Season and AirSeason types for bangumi scheduling
--
-- This module provides types for representing bangumi seasons (Winter, Spring,
-- Summer, Fall) and combined year-season values.
-- These are used to categorize bangumi by their broadcast period.
module Moe.Domain.Bangumi.Season.Types
  ( -- * Season
    Season (..),
    seasonFromText,
    seasonToMonths,

    -- * AirSeason
    AirSeason (..),
    airSeasonFromText,
    airSeasonToText,
    airDateToAirSeason,
    getCurrentAirSeason,

    -- * SeasonNumber
    SeasonNumber (..),
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.OpenApi (Schema (..), ToParamSchema (..), ToSchema (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (getCurrentTime, utctDay)
import Data.Time.Calendar (Day, Year, toGregorian)
import GHC.Generics (Generic)
import Relude (Proxy (..), ToText (..), readMaybe, toString)
import Relude qualified as R
import Web.HttpApiData (FromHttpApiData (..))

-- | Represents the four bangumi broadcast seasons.
--
-- Bangumi seasons follow this schedule:
--
-- * 'Winter' - January to March
-- * 'Spring' - April to June
-- * 'Summer' - July to September
-- * 'Fall' - October to December
data Season = Winter | Spring | Summer | Fall
  deriving stock (Eq, Show, Ord, Enum, Bounded, Generic)
  deriving anyclass (ToSchema)

instance FromHttpApiData Season where
  parseUrlPiece t = case seasonFromText t of
    Just s -> Right s
    Nothing -> Left $ "Invalid season: " <> t

instance ToParamSchema Season where
  toParamSchema _ =
    let base = toParamSchema (Proxy @Text)
     in base {_schemaEnum = Just $ map toJSON [minBound @Season .. maxBound]}

instance ToText Season where
  toText Winter = "Winter"
  toText Spring = "Spring"
  toText Summer = "Summer"
  toText Fall = "Fall"

instance ToJSON Season where
  toJSON = toJSON . toText

-- | An air season combines a year with a season.
--
-- For example, @AirSeason 2024 Winter@ represents the Winter 2024 bangumi season.
data AirSeason = AirSeason
  { year :: Year,
    season :: Season
  }
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (ToJSON, ToSchema)

-- | Season number for multi-season bangumi series.
newtype SeasonNumber = SeasonNumber {unSeasonNumber :: Int}
  deriving stock (Eq, Show, Ord, Generic)
  deriving newtype (ToJSON, FromJSON, ToSchema)

instance FromHttpApiData AirSeason where
  parseUrlPiece t = case airSeasonFromText t of
    Just s -> Right s
    Nothing -> Left $ "Invalid AirSeason: " <> t

instance ToText AirSeason where
  toText = airSeasonToText

-- | Convert an 'AirSeason' to its text representation.
--
-- >>> airSeasonToText (AirSeason 2024 Winter)
-- "2024-Winter"
airSeasonToText :: AirSeason -> Text
airSeasonToText (AirSeason y s) = R.show y <> "-" <> toText s

-- | Parse an 'AirSeason' from text.
--
-- >>> airSeasonFromText "2024-Winter"
-- Just (AirSeason {year = 2024, season = Winter})
--
-- >>> airSeasonFromText "invalid"
-- Nothing
airSeasonFromText :: Text -> Maybe AirSeason
airSeasonFromText t = case T.splitOn "-" t of
  [yearText, seasonText] -> do
    year <- readMaybe (toString yearText)
    season <- seasonFromText seasonText
    pure $ AirSeason year season
  _ -> Nothing

-- | Parse a 'Season' from text.
--
-- >>> seasonFromText "Winter"
-- Just Winter
--
-- >>> seasonFromText "invalid"
-- Nothing
seasonFromText :: Text -> Maybe Season
seasonFromText "Winter" = Just Winter
seasonFromText "Spring" = Just Spring
seasonFromText "Summer" = Just Summer
seasonFromText "Fall" = Just Fall
seasonFromText _ = Nothing

-- | Get the current air season based on system time.
getCurrentAirSeason :: IO AirSeason
getCurrentAirSeason = airDateToAirSeason . utctDay <$> getCurrentTime

-- | Convert an air date to its corresponding air season.
--
-- >>> import Data.Time.Calendar (fromGregorian)
-- >>> airDateToAirSeason (fromGregorian 2024 1 15)
-- AirSeason {year = 2024, season = Winter}
airDateToAirSeason :: Day -> AirSeason
airDateToAirSeason day =
  let (y, m, _) = toGregorian day
   in AirSeason y (monthToSeason m)

monthToSeason :: Int -> Season
monthToSeason m
  | m >= 1 && m <= 3 = Winter
  | m >= 4 && m <= 6 = Spring
  | m >= 7 && m <= 9 = Summer
  | otherwise = Fall

-- | Get the months that belong to a season.
--
-- >>> seasonToMonths Winter
-- [1,2,3]
--
-- >>> seasonToMonths Fall
-- [10,11,12]
seasonToMonths :: Season -> [Int]
seasonToMonths Winter = [1, 2, 3]
seasonToMonths Spring = [4, 5, 6]
seasonToMonths Summer = [7, 8, 9]
seasonToMonths Fall = [10, 11, 12]
