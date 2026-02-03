module Moe.App.Subscription.Fetch
  ( fetchAll,
    fetchAllWithLimit,
    defaultConcurrency,
  )
where

import Control.Concurrent.Async qualified as Async
import Control.Concurrent.QSem (QSem, newQSem, signalQSem, waitQSem)
import Control.Exception (bracket_)
import Data.Map.Strict qualified as Map
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
import Moe.Domain.Bangumi.Types (Bangumi (..), BangumiId)
import Moe.Domain.Tracking.Types (Tracking (..))
import Moe.Infrastructure.Database.Bangumi qualified as BangumiDB
import Moe.Infrastructure.Database.Tracking qualified as DB
import Moe.Infrastructure.Rss.Effect (Rss, RssError, runRss)
import Moe.Prelude
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
  let bids = map (.bangumiId) trackings
  bangumis <- notransact $ BangumiDB.getBangumiByIds bids
  let bangumiMap = buildBangumiMap bangumis
  let rssInfos = mapMaybe (toRssInfo bangumiMap) trackings
  case rssInfos of
    [] -> pure []
    infos -> do
      results <- liftIO $ fetchWithSemaphore concurrency manager infos
      let (successes, failures) = partitionResults results
      mapM_ logFailure failures
      pure successes
  where
    buildBangumiMap :: [Bangumi] -> Map BangumiId Bangumi
    buildBangumiMap = Map.fromList . mapMaybe (\b -> (,b) <$> b.id)

    toRssInfo bMap t = do
      url <- t.rssUrl
      bangumi <- Map.lookup t.bangumiId bMap
      pure (bangumi, url, t.lastPubdate)

    logFailure (bangumi, url, err) =
      Log.logAttention_ $ "RSS fetch failed [" <> display (bangumi.id) <> "] " <> url <> ": " <> display err

type RssInfo = (Bangumi, Text, Maybe UTCTime)
type FetchAttempt = (Bangumi, Text, Either RssError FetchResult)

fetchWithSemaphore ::
  Int ->
  Manager ->
  [RssInfo] ->
  IO [FetchAttempt]
fetchWithSemaphore concurrency manager infos = do
  sem <- newQSem concurrency
  Async.forConcurrently infos $ \(bangumi, url, lastPub) ->
    withSemaphore sem $ do
      result <- runEff $ runRss manager $ runErrorNoCallStack $ fetchSingle bangumi url lastPub
      pure (bangumi, url, result)

withSemaphore :: QSem -> IO a -> IO a
withSemaphore sem = bracket_ (waitQSem sem) (signalQSem sem)

partitionResults :: [FetchAttempt] -> ([FetchResult], [(Bangumi, Text, RssError)])
partitionResults = foldr partition ([], [])
  where
    partition (bangumi, url, Left err) (s, f) = (s, (bangumi, url, err) : f)
    partition (_bangumi, _url, Right fr) (s, f) = (fr : s, f)
