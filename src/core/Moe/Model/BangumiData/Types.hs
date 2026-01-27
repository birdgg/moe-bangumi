module Moe.Model.BangumiData.Types
  ( extractMikanId,
    extractBgmtvId,
    extractTmdbId,
    itemToAnimeSeason,
    toBangumi,
  )
where

import BangumiData.Types (BangumiDataItem (..), BangumiDataSite (..), TitleTranslate (..))
import Data.Time (defaultTimeLocale, parseTimeM)
import Data.Time.Calendar (toGregorian)
import Data.Time.LocalTime (LocalTime (..), localDay)
import Moe.Model.Bangumi.Types qualified as Types

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

itemToAnimeSeason :: BangumiDataItem -> Maybe Types.AnimeSeason
itemToAnimeSeason item = item.begin >>= parseAnimeSeasonFromISO8601

parseAnimeSeasonFromISO8601 :: Text -> Maybe Types.AnimeSeason
parseAnimeSeasonFromISO8601 t = do
  localTime <- parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Q%Z" (toString t)
  let (y, m, _) = toGregorian (localDay (localTime :: LocalTime))
  pure $ Types.animeSeasonFromMonth (fromIntegral y) m

toBangumi :: BangumiDataItem -> Types.Bangumi
toBangumi item =
  Types.Bangumi
    { id = Nothing,
      name = selectName item,
      year = fmap (.year) (itemToAnimeSeason item),
      animeSeason = itemToAnimeSeason item,
      mikanId = extractMikanId item.sites,
      tmdbId = extractTmdbId item.sites,
      bgmtvId = extractBgmtvId item.sites,
      posterUrl = Nothing,
      overview = Nothing
    }

selectName :: BangumiDataItem -> Text
selectName item =
  case item.titleTranslate.zhHans of
    (n : _) -> n
    [] -> case item.titleTranslate.zhHant of
      (n : _) -> n
      [] -> item.title
