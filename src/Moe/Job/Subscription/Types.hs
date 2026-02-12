-- | Types for RSS subscription processing.
module Moe.Job.Subscription.Types
  ( RssContext (..),
    toRssContexts,
  )
where

import Moe.Domain.Bangumi (Bangumi (..))
import Moe.Domain.Rss (PubDate)
import Moe.Domain.Shared.Entity (Entity (..))
import Moe.Domain.Tracking (Tracking (..))
import Data.Text.Display (Display (..))
import Moe.Prelude

-- | Context for processing a single RSS subscription.
data RssContext = RssContext
  { bangumi :: Entity Bangumi,
    rssUrl :: Text,
    lastPubdate :: Maybe PubDate,
    autoComplete :: Bool,
    episodeOffset :: Word32
  }
  deriving stock (Show, Eq)

instance Display RssContext where
  displayBuilder ctx =
    "bangumi: "
      <> displayBuilder (ctx.bangumi.entityVal.titleChs :: Text)
      <> ", rss url: "
      <> displayBuilder ctx.rssUrl

-- | Convert tracking-bangumi pairs to RSS contexts, skipping trackings without RSS URL.
toRssContexts :: [(Entity Tracking, Entity Bangumi)] -> [RssContext]
toRssContexts = mapMaybe toRssContext
  where
    toRssContext (trackingEntity, bangumiEntity) = do
      url <- trackingEntity.entityVal.rssUrl
      pure
        RssContext
          { bangumi = bangumiEntity,
            rssUrl = url,
            lastPubdate = trackingEntity.entityVal.lastPubdate,
            autoComplete = trackingEntity.entityVal.autoComplete,
            episodeOffset = trackingEntity.entityVal.episodeOffset
          }
