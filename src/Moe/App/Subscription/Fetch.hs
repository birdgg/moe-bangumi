module Moe.App.Subscription.Fetch
  ( fetchAll,
    fetchAllWithLimit,
    defaultConcurrency,
  )
where

import Control.Concurrent.Async qualified as Async
import Control.Concurrent.QSem (QSem, newQSem, signalQSem, waitQSem)
import Control.Exception (bracket_)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text.Display (display)
import Data.Time (UTCTime)
import Effectful
import Effectful.Concurrent (Concurrent)
import Effectful.Error.Static (runErrorNoCallStack)
import Effectful.Log (Log)
import Effectful.Log qualified as Log
import Effectful.Sqlite (Sqlite, notransact)
import Moe.App.Subscription.SingleFetch (fetchSingle)
import Moe.App.Subscription.Types (FetchResult (..))
import Moe.Domain.Bangumi.Types (BangumiId)
import Moe.Domain.Tracking.Types (Tracking (..))
import Moe.Infrastructure.Database.Tracking qualified as DB
import Moe.Infrastructure.Rss.Effect (Rss, RssError, runRss)
import Network.HTTP.Client (Manager)

defaultConcurrency :: Int
defaultConcurrency = 5

fetchAll ::
  (Rss :> es, Sqlite :> es, Concurrent :> es, Log :> es, IOE :> es) =>
  Manager ->
  Eff es [FetchResult]
fetchAll = fetchAllWithLimit defaultConcurrency

fetchAllWithLimit ::
  (Rss :> es, Sqlite :> es, Concurrent :> es, Log :> es, IOE :> es) =>
  Int ->
  Manager ->
  Eff es [FetchResult]
fetchAllWithLimit concurrency manager = do
  trackings <- notransact DB.listEnabledRssTracking
  let rssInfos = mapMaybe toRssInfo trackings
  case rssInfos of
    [] -> pure []
    infos -> do
      results <- liftIO $ fetchWithSemaphore concurrency manager infos
      let (successes, failures) = partitionResults results
      mapM_ logFailure failures
      pure successes
  where
    toRssInfo t = case t.rssUrl of
      Just url -> Just (t.bangumiId, url, t.lastPubdate)
      Nothing -> Nothing

    logFailure (bid, url, err) =
      Log.logAttention_ $ "RSS fetch failed [" <> display bid <> "] " <> url <> ": " <> display err

type RssInfo = (BangumiId, Text, Maybe UTCTime)
type FetchAttempt = (BangumiId, Text, Either RssError FetchResult)

fetchWithSemaphore ::
  Int ->
  Manager ->
  [RssInfo] ->
  IO [FetchAttempt]
fetchWithSemaphore concurrency manager infos = do
  sem <- newQSem concurrency
  Async.forConcurrently infos $ \(bid, url, lastPub) ->
    withSemaphore sem $ do
      result <- runEff $ runRss manager $ runErrorNoCallStack $ fetchSingle bid url lastPub
      pure (bid, url, result)

withSemaphore :: QSem -> IO a -> IO a
withSemaphore sem = bracket_ (waitQSem sem) (signalQSem sem)

partitionResults :: [FetchAttempt] -> ([FetchResult], [(BangumiId, Text, RssError)])
partitionResults = foldr partition ([], [])
  where
    partition (bid, url, Left err) (s, f) = (s, (bid, url, err) : f)
    partition (_bid, _url, Right fr) (s, f) = (fr : s, f)
