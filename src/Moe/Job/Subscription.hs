-- | RSS subscription system.
--
-- A dedicated worker thread ('rssWorkerThread') polls 'getSubscriptionContexts'
-- every 30 minutes and also processes items pushed via a 'TQueue' from API handlers.
-- Handlers enqueue work with 'triggerSingleSubscription' instead of spawning async tasks.
module Moe.Job.Subscription
  ( -- * Worker thread
    rssWorkerThread,

    -- * Handler trigger
    triggerSingleSubscription,

    -- * Exported for testing
    filterItems,

    -- * Re-exported types
    module Moe.Job.Subscription.Types,
  )
where

import Moe.Job.Subscription.Filter (filterItems)
import Moe.Job.Subscription.Process (triggerSingleSubscription)
import Moe.Job.Subscription.Types
import Moe.Job.Subscription.Worker (rssWorkerThread)
