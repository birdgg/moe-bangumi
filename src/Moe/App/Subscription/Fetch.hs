module Moe.App.Subscription.Fetch
  ( fetchAll,
    fetchAllWithLimit,
    fetchSingle,
    fetchSingleByBangumiId,
    processSingleFetch,
    SingleFetchResult (..),
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
import Effectful.Error.Static (Error, runErrorNoCallStack)
import Effectful.Log (Log)
import Effectful.Log qualified as Log
import Effectful.Sqlite (Sqlite, notransact, transact)
import Moe.App.Subscription.Download (toDownloadTasks)
import Moe.App.Subscription.Filter (filterItems)
import Moe.App.Subscription.Types (DownloadTask (..), FetchResult (..), FilteredItem (..))
import Moe.Domain.Bangumi.Types (Bangumi (..), BangumiId)
import Moe.Domain.Setting.Types (UserPreference (..))
import Moe.Domain.Tracking.Types (Tracking (..))
import Moe.Domain.Tracking.Types qualified as Tracking
import Moe.Infrastructure.Database.Bangumi qualified as BangumiDB
import Moe.Infrastructure.Database.Tracking qualified as DB
import Moe.Infrastructure.Rss.Effect (Rss, RssError, fetchRss, runRss)
import Moe.Infrastructure.Setting.Effect (Setting, getSetting)
import Moe.Prelude
import Network.HTTP.Client (Manager)

defaultConcurrency :: Int
defaultConcurrency = 5

data SingleFetchResult
  = SingleFetchSuccess [DownloadTask]
  | SingleFetchSkipped Text
  | SingleFetchFailed Text
  deriving stock (Show, Eq)

fetchSingle ::
  (Rss :> es, Error RssError :> es) =>
  Bangumi ->
  Text ->
  Maybe UTCTime ->
  Eff es FetchResult
fetchSingle bangumi url lastPub = do
  items <- fetchRss url
  pure
    FetchResult
      { bangumi = bangumi,
        rssUrl = url,
        lastPubdate = lastPub,
        items = items
      }

fetchSingleByBangumiId ::
  (Rss :> es, Sqlite :> es, Concurrent :> es, Log :> es, IOE :> es) =>
  Manager ->
  BangumiId ->
  Eff es (Either Text FetchResult)
fetchSingleByBangumiId _manager bid = do
  mTracking <- notransact $ DB.getTrackingByBangumi bid
  case mTracking of
    Nothing -> pure $ Left "Tracking not found"
    Just tracking -> case tracking.rssUrl of
      Nothing -> pure $ Left "RSS URL not configured"
      Just url -> do
        mBangumi <- notransact $ BangumiDB.getBangumi bid
        case mBangumi of
          Nothing -> pure $ Left "Bangumi not found"
          Just bangumi -> do
            result <- runErrorNoCallStack $ fetchSingle bangumi url tracking.lastPubdate
            case result of
              Left err -> do
                Log.logAttention_ $ "[" <> display bid <> "] RSS fetch failed: " <> display err
                pure $ Left $ "RSS fetch failed: " <> display err
              Right fr -> pure $ Right fr

processSingleFetch ::
  (Rss :> es, Sqlite :> es, Concurrent :> es, Setting :> es, Log :> es, IOE :> es) =>
  Manager ->
  BangumiId ->
  Eff es SingleFetchResult
processSingleFetch manager bid = do
  fetchResult <- fetchSingleByBangumiId manager bid
  case fetchResult of
    Left errMsg -> pure $ SingleFetchFailed errMsg
    Right fr -> do
      pref <- getSetting
      let filterConfig = pref.filter
      let filteredItems = filterItems filterConfig fr
      let tasks = toDownloadTasks filteredItems
      case tasks of
        [] -> pure $ SingleFetchSkipped "No new items to download"
        _ -> do
          updateLastPubdateForBangumi bid filteredItems
          pure $ SingleFetchSuccess tasks

updateLastPubdateForBangumi ::
  (Sqlite :> es, Concurrent :> es, Log :> es, IOE :> es) =>
  BangumiId ->
  [FilteredItem] ->
  Eff es ()
updateLastPubdateForBangumi bid items = transact $ do
  case latestPubDate items of
    Nothing -> pass
    Just latestPub -> do
      mTracking <- DB.getTrackingByBangumi bid
      case mTracking of
        Nothing -> pass
        Just tracking ->
          DB.updateTracking tracking {Tracking.lastPubdate = Just latestPub}
  where
    latestPubDate :: [FilteredItem] -> Maybe UTCTime
    latestPubDate [] = Nothing
    latestPubDate xs = Just $ maximum $ map (.parsedPubDate) xs

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
