module Moe.App.Subscription.Pipeline
  ( runPipeline,
    module Moe.App.Subscription.Types,
  )
where

import Data.List (nubBy)
import Data.List qualified as List
import Moe.Prelude
import Effectful
import Effectful.Concurrent (Concurrent)
import Effectful.Log (Log)
import Effectful.Log qualified as Log
import Effectful.Sqlite (Sqlite, transact)
import Moe.App.Subscription.Fetch (fetchAll)
import Moe.App.Subscription.Filter (filterFetchResults)
import Moe.App.Subscription.Types
import Moe.App.Subscription.Washing (WashingResult (..), buildEpisodeMap, processWashing)
import Moe.Domain.Bangumi.Episode (Episode (..))
import Moe.Domain.Bangumi.Types (Bangumi (..), BangumiId)
import Moe.Domain.Setting.Types (FilterConfig, UserPreference (..))
import Moe.Infrastructure.Database.Episode qualified as EpisodeDB
import Moe.Infrastructure.Rss.Effect (Rss)
import Moe.Infrastructure.Setting.Effect (Setting, getSetting)
import Network.HTTP.Client (Manager)

runPipeline ::
  (Rss :> es, Setting :> es, Sqlite :> es, Concurrent :> es, Log :> es, IOE :> es) =>
  Manager ->
  Eff es ()
runPipeline manager = do
  Log.logInfo_ "Starting RSS subscription"

  fetchResults <- fetchAll manager

  pref <- getSetting
  let filterConfig = pref.filter
  let filteredItems = filterFetchResults filterConfig fetchResults

  let grouped = groupByBangumi filteredItems
  washingResults <- processAllGroups filterConfig grouped

  let (newTasks, upgradeTasks) = partitionResults washingResults
  processNewEpisodes newTasks
  processUpgrades upgradeTasks

groupByBangumi :: [FilteredItem] -> [(BangumiId, [FilteredItem])]
groupByBangumi items =
  let uniqueBids = mapMaybe (\fi -> fi.bangumi.id) $ nubBy (\a b -> a.bangumi.id == b.bangumi.id) items
   in [(bid, List.filter (\i -> i.bangumi.id == Just bid) items) | bid <- uniqueBids]

processAllGroups ::
  (Sqlite :> es, Concurrent :> es, Log :> es, IOE :> es) =>
  Maybe FilterConfig ->
  [(BangumiId, [FilteredItem])] ->
  Eff es [WashingResult]
processAllGroups _ [] = pure []
processAllGroups filterConfig groups = do
  results <- mapM (processGroup filterConfig) groups
  pure $ concat results

processGroup ::
  (Sqlite :> es, Concurrent :> es, Log :> es, IOE :> es) =>
  Maybe FilterConfig ->
  (BangumiId, [FilteredItem]) ->
  Eff es [WashingResult]
processGroup filterConfig (bid, items) = transact $ do
  episodes <- EpisodeDB.listEpisodesByBangumi bid
  let episodeMap = buildEpisodeMap episodes
  pure $ mapMaybe (processWashing episodeMap filterConfig) items

partitionResults :: [WashingResult] -> ([(DownloadTask, Episode)], [(DownloadTask, Episode, Episode)])
partitionResults = foldr go ([], [])
  where
    go SkipEpisode acc = acc
    go (NewEpisode task ep) (news, ups) = ((task, ep) : news, ups)
    go (UpgradeEpisode task newEp oldEp) (news, ups) = (news, (task, newEp, oldEp) : ups)

processNewEpisodes ::
  (Sqlite :> es, Concurrent :> es, Log :> es, IOE :> es) =>
  [(DownloadTask, Episode)] ->
  Eff es ()
processNewEpisodes tasks = transact $ do
  mapM_ (\(_, ep) -> EpisodeDB.upsertEpisode ep) tasks

processUpgrades ::
  (Sqlite :> es, Concurrent :> es, Log :> es, IOE :> es) =>
  [(DownloadTask, Episode, Episode)] ->
  Eff es ()
processUpgrades tasks = transact $ do
  mapM_ (\(_, newEp, _) -> EpisodeDB.upsertEpisode newEp) tasks
