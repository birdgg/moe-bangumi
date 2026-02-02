module Moe.App.RssSync.Filter
  ( filterItems,
    filterFetchResults,
    parsePubDate,
  )
where

import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime, defaultTimeLocale, parseTimeM)
import Moe.App.RssSync.Types (FetchResult (..), FilteredItem (..))
import Moe.Domain.Bangumi.Types (BangumiId)
import Moe.Domain.Setting.Types (FilterConfig (..))
import Moe.Infrastructure.Rss.Types (RawItem (..))

filterFetchResults :: Maybe FilterConfig -> [FetchResult] -> [FilteredItem]
filterFetchResults config = concatMap (filterItems config)

filterItems :: Maybe FilterConfig -> FetchResult -> [FilteredItem]
filterItems config fr =
  fr.items
    & filter (not . matchesGlobalFilter globalFilters)
    & filter (isNewerThan fr.lastPubdate)
    & mapMaybe (toFilteredItem fr.bangumiId)
  where
    globalFilters = maybe [] (.globalRssFilter) config

matchesGlobalFilter :: [Text] -> RawItem -> Bool
matchesGlobalFilter patterns item =
  case item.title of
    Nothing -> False
    Just title -> any (`T.isInfixOf` title) patterns

isNewerThan :: Maybe UTCTime -> RawItem -> Bool
isNewerThan Nothing _ = True
isNewerThan (Just lastPub) item =
  case item.pubDate >>= parsePubDate of
    Nothing -> True
    Just pubTime -> pubTime > lastPub

toFilteredItem :: BangumiId -> RawItem -> Maybe FilteredItem
toFilteredItem bid item = do
  pubDateStr <- item.pubDate
  pubTime <- parsePubDate pubDateStr
  pure
    FilteredItem
      { bangumiId = bid,
        item = item,
        parsedPubDate = pubTime
      }

parsePubDate :: Text -> Maybe UTCTime
parsePubDate txt =
  let str = T.unpack txt
   in parseRfc822 str
        <|> parseIso8601 str
        <|> parseSimple str
  where
    parseRfc822 = parseTimeM True defaultTimeLocale "%a, %d %b %Y %H:%M:%S %z"
    parseIso8601 = parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%z"
    parseSimple = parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S"

(&) :: a -> (a -> b) -> b
(&) = flip ($)
infixl 1 &

(<|>) :: Maybe a -> Maybe a -> Maybe a
(<|>) Nothing y = y
(<|>) x _ = x
infixl 3 <|>
