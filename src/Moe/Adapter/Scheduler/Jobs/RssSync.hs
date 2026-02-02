module Moe.Adapter.Scheduler.Jobs.RssSync
  ( rssSyncJob,
  )
where

import Control.Concurrent.Async qualified as Async
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text.Display (display)
import Effectful
import Effectful.Log (Logger)
import Effectful.Log qualified as Log
import Effectful.Sqlite (notransact)
import Moe.Adapter.Database.Tracking qualified as DB
import Moe.App.Env (MoeEnv (..))
import Moe.App.Job (RssJobEffects, runRssJob)
import Moe.App.Scheduler (JobDefinition (..))
import Moe.Domain.Bangumi.Types ()
import Moe.Domain.Scheduler.Types (JobResult (..), mkJobConfig)
import Moe.Domain.Tracking.Types (Tracking (..))
import Moe.Effect.Rss qualified as Rss

rssSyncJob :: Text -> MoeEnv -> Logger -> JobDefinition
rssSyncJob cronExpr env logger =
  case mkJobConfig "rss-sync" cronExpr of
    Left err -> error $ "Invalid rss-sync cron: " <> show err
    Right config ->
      JobDefinition
        { jobConfig = config,
          jobAction = runRssJob "rss-sync" logger env (rssSyncAction env)
        }

rssSyncAction :: MoeEnv -> Eff RssJobEffects JobResult
rssSyncAction env = do
  trackings <- notransact DB.listEnabledRssTracking
  let rssUrls = mapMaybe (\t -> (t.bangumiId,) <$> t.rssUrl) trackings
  case rssUrls of
    [] -> do
      Log.logInfo_ "No enabled RSS subscriptions"
      pure JobSuccess
    urls -> do
      Log.logInfo_ "Fetching subscriptions..."
      results <- liftIO $ Async.forConcurrently urls $ \(bid, url) -> do
        result <- runEff $ Rss.runRss env.httpManager $ Rss.fetchRss url
        pure (bid, url, result)
      let (successes, failures) = partitionResults results
      mapM_ logFailure failures
      mapM_ logSuccess successes
      pure JobSuccess
  where
    partitionResults = foldr partition ([], [])
    partition (bid, url, Left err) (s, f) = (s, (bid, url, err) : f)
    partition (bid, url, Right items) (s, f) = ((bid, url, items) : s, f)

    logFailure (bid, url, err) =
      Log.logAttention_ $ "RSS fetch failed [" <> display bid <> "] " <> url <> ": " <> display err

    logSuccess (bid, _url, items) =
      Log.logInfo_ $ "RSS fetch success [" <> display bid <> "] items=" <> display (length items)
