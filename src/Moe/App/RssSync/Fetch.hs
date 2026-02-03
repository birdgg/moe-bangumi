module Moe.App.RssSync.Fetch
  ( fetchAll,
  )
where

import Control.Concurrent.Async qualified as Async
import Data.Maybe (mapMaybe)
import Data.Text.Display (display)
import Effectful
import Effectful.Concurrent (Concurrent)
import Effectful.Error.Static (runErrorNoCallStack)
import Effectful.Log (Log)
import Effectful.Log qualified as Log
import Effectful.Sqlite (Sqlite, notransact)
import Moe.App.RssSync.Types (FetchResult (..))
import Moe.Domain.Tracking.Types (Tracking (..))
import Moe.Infrastructure.Database.Tracking qualified as DB
import Moe.Infrastructure.Rss.Effect (Rss, fetchRss, runRss)
import Network.HTTP.Client (Manager)

fetchAll ::
  (Rss :> es, Sqlite :> es, Concurrent :> es, Log :> es, IOE :> es) =>
  Manager ->
  Eff es [FetchResult]
fetchAll manager = do
  trackings <- notransact DB.listEnabledRssTracking
  let rssInfos = mapMaybe toRssInfo trackings
  case rssInfos of
    [] -> do
      Log.logInfo_ "No enabled RSS subscriptions"
      pure []
    infos -> do
      Log.logInfo_ "Fetching RSS subscriptions..."
      results <- liftIO $ Async.forConcurrently infos $ \(bid, url, lastPub) -> do
        result <- runEff $ runRss manager $ runErrorNoCallStack $ fetchRss url
        pure (bid, url, lastPub, result)
      let (successes, failures) = partitionResults results
      mapM_ logFailure failures
      pure successes
  where
    toRssInfo t = case t.rssUrl of
      Just url -> Just (t.bangumiId, url, t.lastPubdate)
      Nothing -> Nothing

    partitionResults = foldr partition ([], [])

    partition (bid, url, _, Left err) (s, f) = (s, (bid, url, err) : f)
    partition (bid, url, lastPub, Right items) (s, f) =
      (FetchResult bid url lastPub items : s, f)

    logFailure (bid, url, err) =
      Log.logAttention_ $ "RSS fetch failed [" <> display bid <> "] " <> url <> ": " <> display err
