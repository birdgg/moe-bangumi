module Moe.App.Subscription.SingleFetch
  ( fetchSingle,
    fetchSingleByBangumiId,
    processSingleFetch,
    SingleFetchResult (..),
  )
where

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
import Moe.Domain.Bangumi.Types (BangumiId)
import Moe.Domain.Setting.Types (UserPreference (..))
import Moe.Domain.Tracking.Types (Tracking (..))
import Moe.Domain.Tracking.Types qualified as Tracking
import Moe.Infrastructure.Database.Tracking qualified as DB
import Moe.Infrastructure.Rss.Effect (Rss, RssError, fetchRss)
import Moe.Infrastructure.Setting.Effect (Setting, getSetting)
import Moe.Prelude
import Network.HTTP.Client (Manager)

data SingleFetchResult
  = SingleFetchSuccess [DownloadTask]
  | SingleFetchSkipped Text
  | SingleFetchFailed Text
  deriving stock (Show, Eq)

fetchSingle ::
  (Rss :> es, Error RssError :> es) =>
  BangumiId ->
  Text ->
  Maybe UTCTime ->
  Eff es FetchResult
fetchSingle bid url lastPub = do
  items <- fetchRss url
  pure
    FetchResult
      { bangumiId = bid,
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
        result <- runErrorNoCallStack $ fetchSingle bid url tracking.lastPubdate
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
