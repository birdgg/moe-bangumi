module Moe.Infrastructure.BangumiData.Types
  ( BangumiDataItem (..),
    BangumiDataSite (..),
    TitleTranslate (..),
    extractMikanId,
    extractBgmtvId,
    extractTmdbId,
    toBangumi,
  )
where

import Control.Monad ((>=>))
import Data.Aeson
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (defaultTimeLocale, parseTimeM)
import Data.Time.Calendar (Day)
import Data.Time.LocalTime (LocalTime (..), localDay)
import Data.Word (Word32)
import Moe.Domain.Bangumi.Types qualified as Types
import Text.Read (readMaybe)

data BangumiDataItem = BangumiDataItem
  { title :: Text,
    titleTranslate :: TitleTranslate,
    itemType :: Types.BangumiKind,
    lang :: Text,
    begin :: Maybe Text,
    sites :: [BangumiDataSite]
  }
  deriving stock (Eq, Show)

instance FromJSON BangumiDataItem where
  parseJSON = withObject "BangumiDataItem" $ \v ->
    BangumiDataItem
      <$> v .: "title"
      <*> v .:? "titleTranslate" .!= emptyTitleTranslate
      <*> (fromMaybe Types.Tv . Types.bangumiKindFromText <$> v .: "type")
      <*> v .: "lang"
      <*> v .:? "begin"
      <*> v .:? "sites" .!= []

data BangumiDataSite = BangumiDataSite
  { site :: Text,
    siteId :: Text
  }
  deriving stock (Eq, Show)

instance FromJSON BangumiDataSite where
  parseJSON = withObject "BangumiDataSite" $ \v ->
    BangumiDataSite
      <$> v .: "site"
      <*> v .: "id"

data TitleTranslate = TitleTranslate
  { zhHans :: [Text],
    zhHant :: [Text]
  }
  deriving stock (Eq, Show)

emptyTitleTranslate :: TitleTranslate
emptyTitleTranslate = TitleTranslate [] []

instance FromJSON TitleTranslate where
  parseJSON = withObject "TitleTranslate" $ \v ->
    TitleTranslate
      <$> v .:? "zh-Hans" .!= []
      <*> v .:? "zh-Hant" .!= []

extractMikanId :: [BangumiDataSite] -> Maybe Types.MikanId
extractMikanId = findSiteId "mikan" >=> parseId Types.MikanId

extractBgmtvId :: [BangumiDataSite] -> Maybe Types.BgmtvId
extractBgmtvId = findSiteId "bangumi" >=> parseId Types.BgmtvId

extractTmdbId :: [BangumiDataSite] -> Maybe Types.TmdbId
extractTmdbId = findSiteId "tmdb" >=> parseTmdbId

parseTmdbId :: Text -> Maybe Types.TmdbId
parseTmdbId t = case T.splitOn "/" t of
  [_, idPart] -> Types.TmdbId <$> readMaybe (T.unpack idPart)
  [_, idPart, _, _] -> Types.TmdbId <$> readMaybe (T.unpack idPart)
  _ -> Types.TmdbId <$> readMaybe (T.unpack t)

findSiteId :: Text -> [BangumiDataSite] -> Maybe Text
findSiteId siteName = fmap (.siteId) . find (\s -> s.site == siteName)

parseId :: (Word32 -> a) -> Text -> Maybe a
parseId constructor t = constructor <$> readMaybe (T.unpack t)

parseAirDate :: BangumiDataItem -> Maybe Day
parseAirDate item = item.begin >>= parseAirDateFromISO8601

parseAirDateFromISO8601 :: Text -> Maybe Day
parseAirDateFromISO8601 t = do
  localTime <- parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Q%Z" (T.unpack t)
  pure $ localDay (localTime :: LocalTime)

toBangumi :: BangumiDataItem -> Types.Bangumi
toBangumi item =
  Types.Bangumi
    { id = Nothing,
      titleChs = selectChsName item,
      titleJap = Just item.title,
      airDate = parseAirDate item,
      season = Nothing,
      kind = item.itemType,
      mikanId = extractMikanId item.sites,
      tmdbId = extractTmdbId item.sites,
      bgmtvId = extractBgmtvId item.sites,
      posterUrl = Nothing
    }

selectChsName :: BangumiDataItem -> Text
selectChsName item =
  case item.titleTranslate.zhHans of
    (n : _) -> n
    [] -> case item.titleTranslate.zhHant of
      (n : _) -> n
      [] -> item.title
