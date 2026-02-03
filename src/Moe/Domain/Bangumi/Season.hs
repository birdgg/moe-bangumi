module Moe.Domain.Bangumi.Season
  ( Season (..),
    seasonFromText,
    seasonToMonths,
    AirSeason (..),
    airSeasonFromText,
    airSeasonToText,
    airDateToAirSeason,
    getCurrentAirSeason,
  )
where

import Data.Aeson (ToJSON (..))
import Data.OpenApi (Schema (..), ToParamSchema (..), ToSchema (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (getCurrentTime, utctDay)
import Data.Time.Calendar (Day, Year, toGregorian)
import GHC.Generics (Generic)
import Relude (Proxy (..), ToText (..), readMaybe, toString)
import Relude qualified as R
import Web.HttpApiData (FromHttpApiData (..))

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

data AirSeason = AirSeason
  { year :: Year,
    season :: Season
  }
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (ToJSON, ToSchema)

instance FromHttpApiData AirSeason where
  parseUrlPiece t = case airSeasonFromText t of
    Just s -> Right s
    Nothing -> Left $ "Invalid AirSeason: " <> t

instance ToText AirSeason where
  toText = airSeasonToText

airSeasonToText :: AirSeason -> Text
airSeasonToText (AirSeason y s) = R.show y <> "-" <> toText s

airSeasonFromText :: Text -> Maybe AirSeason
airSeasonFromText t = case T.splitOn "-" t of
  [yearText, seasonText] -> do
    year <- readMaybe (toString yearText)
    season <- seasonFromText seasonText
    pure $ AirSeason year season
  _ -> Nothing

seasonFromText :: Text -> Maybe Season
seasonFromText "Winter" = Just Winter
seasonFromText "Spring" = Just Spring
seasonFromText "Summer" = Just Summer
seasonFromText "Fall" = Just Fall
seasonFromText _ = Nothing

getCurrentAirSeason :: IO AirSeason
getCurrentAirSeason = airDateToAirSeason . utctDay <$> getCurrentTime

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

seasonToMonths :: Season -> [Int]
seasonToMonths Winter = [1, 2, 3]
seasonToMonths Spring = [4, 5, 6]
seasonToMonths Summer = [7, 8, 9]
seasonToMonths Fall = [10, 11, 12]
