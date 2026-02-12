-- | RSS item filtering for subscriptions.
module Moe.Job.Subscription.Filter
  ( filterItems,
    selectEpisodes,
  )
where

import Data.List qualified as List
import Moe.Domain.Episode (Episode (..))
import Moe.Domain.Parser.Internal.Pattern (Pattern, matchPattern, mkPattern)
import Moe.Domain.Rss (PubDate)
import Moe.Domain.Setting (FilterConfig (..))
import Moe.Domain.Shared.Numbering (EpisodeNumber)
import Moe.Infra.Rss.Types (RawItem (..))
import Moe.Job.Subscription.Types (RssContext (..))
import Moe.Prelude

-- | Filter raw items: remove global-filtered, keep only newer than last pubdate, require parsed pubDate.
-- Compiles regex patterns once and applies all predicates in a single pass.
filterItems :: FilterConfig -> RssContext -> [RawItem] -> [RawItem]
filterItems config ctx =
  let compiled = map mkPattern config.globalRssFilter
   in List.filter $ \item ->
        isJust item.pubDate
          && not (matchesGlobalFilter compiled item)
          && isNewerThan ctx.lastPubdate item

matchesGlobalFilter :: [Pattern] -> RawItem -> Bool
matchesGlobalFilter compiled item =
  case item.title of
    Nothing -> False
    Just title -> any (`matchPattern` title) compiled

-- | Check if an item is newer than the last seen pubdate.
-- Note: items without pubDate are already filtered out by 'filterItems',
-- so the inner Nothing case is unreachable in practice.
isNewerThan :: Maybe PubDate -> RawItem -> Bool
isNewerThan Nothing _ = True
isNewerThan (Just lastPub) item =
  case item.pubDate of
    Nothing -> False
    Just pubTime -> pubTime > lastPub

-- | When autoComplete is disabled, keep only episodes with the highest number.
selectEpisodes :: Bool -> [Episode] -> [Episode]
selectEpisodes True eps = eps
selectEpisodes False [] = []
selectEpisodes False (e : es) =
  let maxNum :: EpisodeNumber
      maxNum = foldl' (\m x -> max m x.episodeNumber) e.episodeNumber es
   in [x | x <- e : es, x.episodeNumber == maxNum]
