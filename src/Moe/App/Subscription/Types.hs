module Moe.App.Subscription.Types
  ( RssContext (..),
    toRssContexts,
  )
where

import Moe.Domain.Bangumi.Types (Bangumi)
import Moe.Domain.Rss.Types (PubDate)
import Moe.Domain.Tracking.Types (Tracking (..))
import Moe.Prelude

-- | Context for processing a single RSS subscription
data RssContext = RssContext
  { bangumi :: Bangumi,
    rssUrl :: Text,
    lastPubdate :: Maybe PubDate
  }
  deriving stock (Show, Eq)

-- | Convert tracking-bangumi pairs to RSS contexts, skipping trackings without RSS URL
toRssContexts :: [(Tracking, Bangumi)] -> [RssContext]
toRssContexts = mapMaybe toRssContext
  where
    toRssContext (tracking, bangumi) = do
      url <- tracking.rssUrl
      pure
        RssContext
          { bangumi = bangumi,
            rssUrl = url,
            lastPubdate = tracking.lastPubdate
          }
