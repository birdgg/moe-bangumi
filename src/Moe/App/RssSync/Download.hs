module Moe.App.RssSync.Download
  ( downloadAll,
    toDownloadTasks,
  )
where

import Data.List (sortOn)
import Data.Maybe (mapMaybe)
import Data.Ord (Down (..))
import Data.Text.Display (display)
import Effectful
import Effectful.Concurrent (Concurrent)
import Effectful.Log (Log)
import Effectful.Log qualified as Log
import Effectful.Sqlite (Sqlite, transact)
import Moe.App.RssSync.Types (DownloadTask (..), FilteredItem (..))
import Moe.Domain.Bangumi.Types (BangumiId)
import Moe.Domain.Tracking.Types qualified as Tracking
import Moe.Infrastructure.Database.Tracking qualified as DB
import Moe.Infrastructure.Rss.Types (RawItem (..))

downloadAll ::
  (Sqlite :> es, Concurrent :> es, Log :> es, IOE :> es) =>
  [FilteredItem] ->
  Eff es [DownloadTask]
downloadAll filteredItems = do
  let tasks = toDownloadTasks filteredItems
  case tasks of
    [] -> do
      Log.logInfo_ "No new items to download"
      pure []
    _ -> do
      Log.logInfo_ $ "Found " <> display (length tasks) <> " items to download"
      mapM_ logTask tasks
      updateLastPubdates filteredItems
      pure tasks
  where
    logTask task =
      Log.logInfo_ $ "[" <> display task.bangumiId <> "] " <> task.torrentUrl

toDownloadTasks :: [FilteredItem] -> [DownloadTask]
toDownloadTasks = mapMaybe toTask
  where
    toTask fi = do
      url <- fi.item.torrentUrl
      pure
        DownloadTask
          { bangumiId = fi.bangumiId,
            torrentUrl = url,
            infoHash = fi.item.infoHash,
            pubDate = fi.parsedPubDate
          }

updateLastPubdates ::
  (Sqlite :> es, Concurrent :> es, Log :> es, IOE :> es) =>
  [FilteredItem] ->
  Eff es ()
updateLastPubdates items = transact $ do
  let grouped = groupByBangumi items
  mapM_ updateTracking grouped
  where
    groupByBangumi :: [FilteredItem] -> [(BangumiId, FilteredItem)]
    groupByBangumi = map selectLatest . groupBy' (.bangumiId)

    selectLatest :: [FilteredItem] -> (BangumiId, FilteredItem)
    selectLatest xs =
      let sorted = sortOn (Down . (.parsedPubDate)) xs
          latest = head sorted
       in (latest.bangumiId, latest)

    groupBy' :: (Eq k) => (a -> k) -> [a] -> [[a]]
    groupBy' _ [] = []
    groupBy' f (x : xs) =
      let (same, rest) = span (\y -> f x == f y) xs
       in (x : same) : groupBy' f rest

    updateTracking (bid, latest) = do
      mTracking <- DB.getTrackingByBangumi bid
      case mTracking of
        Nothing -> pure ()
        Just tracking ->
          DB.updateTracking tracking {Tracking.lastPubdate = Just latest.parsedPubDate}
