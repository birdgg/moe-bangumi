module Moe.Domain.Bangumi
  ( AirDate,
    BangumiId,
    TmdbId (..),
    BgmtvId (..),
    MikanId (..),
    AirSeason (..),
    Season (..),
    SeasonNumber (..),
    BangumiKind (..),
    Bangumi (..),
    airDateToAirSeason,
    seasonToMonths,
    airSeasonFromText,
    getCurrentAirSeason,
    getAirSeason,
    extractYear,
    extractYearText,
  )
where

import Data.Aeson (ToJSON (..))
import Data.Text qualified as T
import Data.OpenApi (NamedSchema (..), OpenApiType (..), Schema (..), ToSchema (..))
import Data.Time.Calendar (Day, Year, toGregorian)
import Effectful.Sqlite (FromField (..), FromRow (..), ToField (..), ToRow (..))
import Moe.Domain.Shared.Metadata
import Moe.Domain.Shared.Numbering (SeasonNumber (..))
import Moe.Domain.Bangumi.Season
import Moe.Domain.Shared.Entity (Id)
import Moe.Orphans ()
import Moe.Prelude

type AirDate = Day

type BangumiId = Id Bangumi

-- TODO: remove Web and Ova kind
data BangumiKind = Tv | Web | Movie | Ova
  deriving stock (Eq, Show, Ord, Enum, Bounded, Generic)

instance FromText BangumiKind where
  fromText = inverseMap toText . T.toLower

instance ToSchema BangumiKind where
  declareNamedSchema _ = do
    let enumValues = map toJSON [minBound @BangumiKind .. maxBound]
    pure $
      NamedSchema (Just "BangumiKind") $
        mempty
          { _schemaType = Just OpenApiString,
            _schemaEnum = Just enumValues
          }

instance ToText BangumiKind where
  toText Tv = "tv"
  toText Web = "web"
  toText Movie = "movie"
  toText Ova = "ova"

instance ToJSON BangumiKind where
  toJSON = toJSON . toText


instance FromField BangumiKind where
  fromField f = do
    t <- fromField @Text f
    case fromText t of
      Just k -> pure k
      Nothing -> fail $ "Invalid kind: " <> toString t

instance ToField BangumiKind where
  toField = toField . toText

data Bangumi = Bangumi
  { titleChs :: Text,
    titleJap :: Maybe Text,
    airDate :: AirDate,
    season :: Maybe SeasonNumber,
    kind :: BangumiKind,
    mikanId :: Maybe MikanId,
    tmdbId :: Maybe TmdbId,
    bgmtvId :: Maybe BgmtvId,
    posterUrl :: Maybe Text,
    totalEpisodes :: Maybe Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromRow, ToRow)

getAirSeason :: Bangumi -> AirSeason
getAirSeason b = airDateToAirSeason b.airDate

-- | Extract year from an air date.
extractYear :: AirDate -> Year
extractYear day = let (y, _, _) = toGregorian day in y

-- | Parse year from text like "2024-01-01".
extractYearText :: Text -> Maybe Year
extractYearText = readMaybe . toString . T.take 4
