module Moe.App.Subscription.Download
  ( downloadAll,
    toDownloadTasks,
  )
where

import Data.List.NonEmpty qualified as NE
import Effectful
import Effectful.Concurrent (Concurrent)
import Effectful.Log (Log)
import Effectful.Sqlite (Sqlite, transact)
import Moe.App.Subscription.Types (DownloadTask (..), FilteredItem (..))
import Moe.Domain.Bangumi.Types (Bangumi (..), BangumiId)
import Moe.Domain.Tracking.Types qualified as Tracking
import Moe.Infrastructure.Database.Tracking qualified as DB
import Moe.Infrastructure.Rss.Types (RawItem (..))
import Moe.Prelude

downloadAll ::
  (Sqlite :> es, Concurrent :> es, Log :> es, IOE :> es) =>
  [FilteredItem] ->
  Eff es [DownloadTask]
downloadAll filteredItems = do
  let tasks = toDownloadTasks filteredItems
  case tasks of
    [] -> pure []
    _ -> do
      updateLastPubdates filteredItems
      pure tasks

toDownloadTasks :: [FilteredItem] -> [DownloadTask]
toDownloadTasks = mapMaybe toTask
  where
    toTask fi = do
      url <- fi.item.torrentUrl
      pure
        DownloadTask
          { bangumi = fi.bangumi,
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
    groupByBangumi = mapMaybe selectLatest . groupBy' (\fi -> fi.bangumi.id)

    selectLatest :: [FilteredItem] -> Maybe (BangumiId, FilteredItem)
    selectLatest xs = do
      let sorted = sortOn (Down . (.parsedPubDate)) xs
          latest = NE.head (NE.fromList sorted)
      bid <- latest.bangumi.id
      pure (bid, latest)

    groupBy' :: (Eq k) => (a -> k) -> [a] -> [[a]]
    groupBy' _ [] = []
    groupBy' f (x : xs) =
      let (same, rest) = span (\y -> f x == f y) xs
       in (x : same) : groupBy' f rest

    updateTracking (bid, latest) = do
      mTracking <- DB.getTrackingByBangumi bid
      case mTracking of
        Nothing -> pass
        Just tracking ->
          DB.updateTracking tracking {Tracking.lastPubdate = Just latest.parsedPubDate}
