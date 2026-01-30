module Moe.Infra.BangumiData.Types
  ( BangumiDataItem (..),
    BangumiDataSite (..),
    TitleTranslate (..),
    extractMikanId,
    extractBgmtvId,
    extractTmdbId,
    toBangumi,
  )
where

import Data.Aeson
import Data.Time (defaultTimeLocale, parseTimeM)
import Data.Time.Calendar (Day)
import Data.Time.LocalTime (LocalTime (..), localDay)
import Moe.Domain.Bangumi.Types qualified as Types

data BangumiDataItem = BangumiDataItem
  { title :: Text,
    titleTranslate :: TitleTranslate,
    itemType :: Text,
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
      <*> v .: "type"
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
extractTmdbId = findSiteId "themoviedb" >=> parseId Types.TmdbId

findSiteId :: Text -> [BangumiDataSite] -> Maybe Text
findSiteId siteName = fmap (.siteId) . find (\s -> s.site == siteName)

parseId :: (Word32 -> a) -> Text -> Maybe a
parseId constructor t = constructor <$> readMaybe (toString t)

parseAirDate :: BangumiDataItem -> Maybe Day
parseAirDate item = item.begin >>= parseAirDateFromISO8601

parseAirDateFromISO8601 :: Text -> Maybe Day
parseAirDateFromISO8601 t = do
  localTime <- parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Q%Z" (toString t)
  pure $ localDay (localTime :: LocalTime)

toBangumi :: BangumiDataItem -> Types.Bangumi
toBangumi item =
  Types.Bangumi
    { id = Nothing,
      name = selectName item,
      airDate = parseAirDate item,
      mikanId = extractMikanId item.sites,
      tmdbId = extractTmdbId item.sites,
      bgmtvId = extractBgmtvId item.sites,
      posterUrl = Nothing
    }

selectName :: BangumiDataItem -> Text
selectName item =
  case item.titleTranslate.zhHans of
    (n : _) -> n
    [] -> case item.titleTranslate.zhHant of
      (n : _) -> n
      [] -> item.title
