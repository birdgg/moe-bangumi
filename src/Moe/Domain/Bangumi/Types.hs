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
    Bangumi (..),
    airDateToAirSeason,
    seasonToMonths,
    seasonFromText,
    airSeasonFromText,
    airSeasonToText,
    getCurrentAirSeason,
    getAirSeason,
  )
where

import Data.Aeson (ToJSON (..))
import Data.Int (Int64)
import Data.OpenApi (NamedSchema (..), OpenApiType (..), Schema (..), ToSchema (..))
import Data.Text (Text)
import Data.Text.Display (Display (..))
import Data.Time.Calendar (Day)
import Data.Word (Word32)
import GHC.Generics (Generic)
import Moe.Domain.Bangumi.Internal.Metadata
import Moe.Domain.Bangumi.Internal.Season
import Moe.Domain.Bangumi.Season
import Moe.Orphans ()
import Relude (ToText (..), inverseMap)

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

data Bangumi = Bangumi
  { id :: Maybe BangumiId,
    titleChs :: Text,
    titleJap :: Maybe Text,
    airDate :: Maybe Day,
    season :: Maybe Word32,
    kind :: BangumiKind,
    mikanId :: Maybe MikanId,
    tmdbId :: Maybe TmdbId,
    bgmtvId :: Maybe BgmtvId,
    posterUrl :: Maybe Text
  }
  deriving stock (Eq, Show)

getAirSeason :: Bangumi -> Maybe AirSeason
getAirSeason b = airDateToAirSeason <$> b.airDate
