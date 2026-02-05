module Moe.Domain.Bangumi.Types
  ( BangumiId (..),
    TmdbId (..),
    BgmtvId (..),
    MikanId (..),
    AirSeason (..),
    Season (..),
    SeasonNumber (..),
    BangumiKind (..),
    bangumiKindFromText,
    BangumiF (..),
    NewBangumi,
    Bangumi,
    withId,
    airDateToAirSeason,
    seasonToMonths,
    seasonFromText,
    airSeasonFromText,
    airSeasonToText,
    getCurrentAirSeason,
    getAirSeason,
    extractYear,
  )
where

import Data.Aeson (ToJSON (..))
import Data.OpenApi (NamedSchema (..), OpenApiType (..), Schema (..), ToSchema (..))
import Data.Text.Display (Display (..))
import Data.Time (UTCTime)
import Data.Time.Calendar (Day, Year, toGregorian)
import Moe.Domain.Bangumi.Internal.Metadata
import Moe.Domain.Bangumi.Internal.Season
import Moe.Domain.Bangumi.Season
import Moe.Orphans ()
import Moe.Prelude

newtype BangumiId = BangumiId Int64
  deriving stock (Eq, Show, Ord)

instance Display BangumiId where
  displayBuilder (BangumiId i) = displayBuilder i

data BangumiKind = Tv | Web | Movie | Ova
  deriving stock (Eq, Show, Ord, Enum, Bounded, Generic)

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

bangumiKindFromText :: Text -> Maybe BangumiKind
bangumiKindFromText = inverseMap toText

data BangumiF id ts = Bangumi
  { id :: id,
    titleChs :: Text,
    titleJap :: Maybe Text,
    airDate :: Maybe Day,
    season :: Maybe SeasonNumber,
    kind :: BangumiKind,
    mikanId :: Maybe MikanId,
    tmdbId :: Maybe TmdbId,
    bgmtvId :: Maybe BgmtvId,
    posterUrl :: Maybe Text,
    createdAt :: ts
  }
  deriving stock (Eq, Show)

type NewBangumi = BangumiF () ()

type Bangumi = BangumiF BangumiId UTCTime

withId :: BangumiId -> UTCTime -> NewBangumi -> Bangumi
withId bid ts b = b {id = bid, createdAt = ts}

getAirSeason :: BangumiF id ts -> Maybe AirSeason
getAirSeason b = airDateToAirSeason <$> b.airDate

-- | Extract year from a Day value.
extractYear :: Day -> Year
extractYear day = let (y, _, _) = toGregorian day in y
