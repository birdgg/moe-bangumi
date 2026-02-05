module Moe.App.Subscription.Filter
  ( filterItems,
  )
where

import Moe.App.Subscription.Types (RssContext (..))
import Moe.Domain.Bangumi.Parser.Internal.Pattern (matchPattern, mkPattern)
import Moe.Domain.Rss.Types (PubDate)
import Moe.Domain.Setting.Types (FilterConfig (..), Regex)
import Moe.Infrastructure.Rss.Types (RawItem (..))
import Moe.Prelude

-- | Filter raw items: remove global-filtered, keep only newer than last pubdate, require parsed pubDate
filterItems :: Maybe FilterConfig -> RssContext -> [RawItem] -> [RawItem]
filterItems config ctx items =
  items
    & filter (not . matchesGlobalFilter globalFilters)
    & filter (isNewerThan ctx.lastPubdate)
    & filter (isJust . (.pubDate))
  where
    globalFilters = maybe [] (.globalRssFilter) config

matchesGlobalFilter :: [Regex] -> RawItem -> Bool
matchesGlobalFilter patterns item =
  case item.title of
    Nothing -> False
    Just title -> any (\p -> matchPattern (mkPattern (encodeUtf8 p)) title) patterns

isNewerThan :: Maybe PubDate -> RawItem -> Bool
isNewerThan Nothing _ = True
isNewerThan (Just lastPub) item =
  case item.pubDate of
    Nothing -> True
    Just pubTime -> pubTime > lastPub
