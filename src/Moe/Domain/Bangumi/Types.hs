module Moe.Domain.Bangumi.Types
  ( BangumiId (..),
    TmdbId (..),
    BgmtvId (..),
    MikanId (..),
    BangumiSeason (..),
    Season (..),
    BangumiKind (..),
    bangumiKindFromText,
    Bangumi (..),
    airDateToBangumiSeason,
    seasonToMonths,
    seasonFromText,
    bangumiSeasonFromText,
    bangumiSeasonToText,
    getCurrentBangumiSeason,
    getBangumiSeason,
  )
where

import Data.Aeson (ToJSON (..))
import Data.Int (Int64)
import Data.OpenApi (ToParamSchema (..), ToSchema (..), Schema (..), NamedSchema (..), OpenApiType (..))
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Conversions (ToText (..))
import Data.Text.Display (Display (..))
import Data.Time (getCurrentTime, utctDay)
import GHC.Generics (Generic)
import Moe.Prelude (inverseMap)
import Data.Time.Calendar (Day, Year, toGregorian)
import Data.Word (Word32)
import Moe.Orphans ()
import Text.Read (readMaybe)
import Web.HttpApiData (FromHttpApiData (..))

newtype BangumiId = BangumiId Int64
  deriving stock (Eq, Show)

instance Display BangumiId where
  displayBuilder (BangumiId i) = displayBuilder i

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

data BangumiSeason = BangumiSeason
  { year :: Year,
    season :: Season
  }
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (ToJSON, ToSchema)

instance FromHttpApiData BangumiSeason where
  parseUrlPiece t = case bangumiSeasonFromText t of
    Just s -> Right s
    Nothing -> Left $ "Invalid BangumiSeason: " <> t

instance ToText BangumiSeason where
  toText = bangumiSeasonToText

bangumiSeasonToText :: BangumiSeason -> Text
bangumiSeasonToText (BangumiSeason y s) = toText y <> "-" <> toText s

bangumiSeasonFromText :: Text -> Maybe BangumiSeason
bangumiSeasonFromText t = case T.splitOn "-" t of
  [yearText, seasonText] -> do
    year <- readMaybe (T.unpack yearText)
    season <- seasonFromText seasonText
    pure $ BangumiSeason year season
  _ -> Nothing

seasonFromText :: Text -> Maybe Season
seasonFromText "Winter" = Just Winter
seasonFromText "Spring" = Just Spring
seasonFromText "Summer" = Just Summer
seasonFromText "Fall" = Just Fall
seasonFromText _ = Nothing

getCurrentBangumiSeason :: IO BangumiSeason
getCurrentBangumiSeason = airDateToBangumiSeason . utctDay <$> getCurrentTime

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

data BangumiKind = Tv | Web | Movie | Ova
  deriving stock (Eq, Show, Ord, Enum, Bounded, Generic)

instance ToSchema BangumiKind where
  declareNamedSchema _ = do
    let enumValues = map toJSON [minBound @BangumiKind .. maxBound]
    pure $ NamedSchema (Just "BangumiKind") $ mempty
      { _schemaType = Just OpenApiString
      , _schemaEnum = Just enumValues
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
    seasonNumber :: Maybe Word32,
    kind :: BangumiKind,
    mikanId :: Maybe MikanId,
    tmdbId :: Maybe TmdbId,
    bgmtvId :: Maybe BgmtvId,
    posterUrl :: Maybe Text
  }
  deriving stock (Eq, Show)

getBangumiSeason :: Bangumi -> Maybe BangumiSeason
getBangumiSeason b = airDateToBangumiSeason <$> b.airDate
