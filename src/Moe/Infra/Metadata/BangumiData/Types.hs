module Moe.Infra.Metadata.BangumiData.Types
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
import Data.Text qualified as T
import Data.Time (defaultTimeLocale, parseTimeM)
import Data.Time.Calendar (Day)
import Data.Time.LocalTime (LocalTime (..), localDay)
import Moe.Domain.Bangumi qualified as Types
import Moe.Domain.Parser.OriginalTitle (ParsedTitle (..), parseOriginalTitle)
import Moe.Prelude

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
      <*> v .:? "titleTranslate" .!= TitleTranslate [] []
      <*> (fromMaybe Types.Tv . (fromText :: Text -> Maybe Types.BangumiKind) <$> v .: "type")
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
  [_, idPart] -> Types.TmdbId <$> readMaybe (toString idPart)
  [_, idPart, _, _] -> Types.TmdbId <$> readMaybe (toString idPart)
  _ -> Types.TmdbId <$> readMaybe (toString t)

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

toBangumi :: BangumiDataItem -> Maybe Types.Bangumi
toBangumi item = do
  date <- parseAirDate item
  let parsed = parseOriginalTitle (item.title, selectChsName item)
  pure Types.Bangumi
    { titleChs = parsed.titleChs,
      titleJap = Just parsed.titleJap,
      airDate = date,
      firstAirYear = Nothing,
      season = parsed.season,
      kind = item.itemType,
      mikanId = extractMikanId item.sites,
      tmdbId = extractTmdbId item.sites,
      bgmtvId = extractBgmtvId item.sites,
      posterUrl = Nothing,
      totalEpisodes = Nothing
    }

selectChsName :: BangumiDataItem -> Text
selectChsName item =
  fromMaybe item.title $
    viaNonEmpty head item.titleTranslate.zhHans
      <|> viaNonEmpty head item.titleTranslate.zhHant
