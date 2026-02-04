module Moe.App.Subscription.Pipeline
  ( runPipeline,
    module Moe.App.Subscription.Types,
  )
where

import Data.List (nubBy)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Effectful
import Effectful.Concurrent (Concurrent)
import Effectful.Log (Log)
import Effectful.Log qualified as Log
import Effectful.Sqlite (Sqlite, transact)
import Effectful.Error.Static (Error)
import Moe.App.Subscription.Fetch (fetchAll)
import Moe.App.Subscription.Filter (filterFetchResults)
import Moe.App.Subscription.Types
import Moe.App.Subscription.Washing (WashingResult (..), buildEpisodeMap, processWashing)
import Moe.Domain.Bangumi.Episode (Episode (..))
import Moe.Domain.Bangumi.Types (Bangumi (..), BangumiId)
import Moe.Domain.Setting.Types (FilterConfig, UserPreference (..))
import Moe.Error (MoeError)
import Moe.Infrastructure.Database.Episode qualified as EpisodeDB
import Moe.Infrastructure.Download.Effect
import Moe.Infrastructure.Rss.Effect (Rss)
import Moe.Infrastructure.Setting.Effect (Setting, getSetting)
import Moe.Prelude
import Network.HTTP.Client (Manager)

runPipeline ::
  (Rss :> es, Download :> es, Error MoeError :> es, Setting :> es, Sqlite :> es, Concurrent :> es, Log :> es, IOE :> es) =>
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

-- | Process new episodes: add torrent and save to database
processNewEpisodes ::
  (Download :> es, Sqlite :> es, Concurrent :> es, Log :> es, IOE :> es) =>
  [(DownloadTask, Episode)] ->
  Eff es ()
processNewEpisodes [] = pass
processNewEpisodes tasks = do
  -- Add torrents to downloader
  forM_ tasks $ \(task, ep) -> do
    let tags = TagList [Moe, Subscription]
    addTorrent task.torrentUrl Nothing (Just tags)
    Log.logInfo_ $ "Added torrent: " <> toText ep.episodeNumber

  -- Save episodes to database
  transact $ mapM_ (\(_, ep) -> EpisodeDB.upsertEpisode ep) tasks

-- | Process upgrades: stop old, add new, tag old for deletion
processUpgrades ::
  (Download :> es, Sqlite :> es, Concurrent :> es, Log :> es, IOE :> es) =>
  [(DownloadTask, Episode, Episode)] ->
  Eff es ()
processUpgrades [] = pass
processUpgrades tasks = do
  -- Get old torrent hashes
  let oldHashes = map (\(_, _, oldEp) -> oldEp.infoHash) tasks

  -- Query old torrents status from downloader
  oldTorrents <- getTorrentsByHashes oldHashes
  let torrentMap = Map.fromList [(t.hash, t) | t <- oldTorrents]

  -- Process each upgrade
  forM_ tasks $ \(task, newEp, oldEp) -> do
    let mOldTorrent = Map.lookup oldEp.infoHash torrentMap

    -- Handle old torrent if exists
    case mOldTorrent of
      Nothing ->
        Log.logInfo_ $ "Old torrent not in downloader: " <> toText oldEp.episodeNumber
      Just oldTorrent -> do
        if isDownloading oldTorrent
          then do
            -- Stop downloading old torrent
            stopTorrents [oldEp.infoHash]
            Log.logInfo_ $ "Stopped old downloading torrent: " <> toText oldEp.episodeNumber
          else
            Log.logInfo_ $ "Old torrent already completed: " <> toText oldEp.episodeNumber

        -- Tag old torrent for deletion
        tagOldForDeletion oldEp.infoHash

    -- Always add new torrent
    let tags = TagList [Moe, Subscription]
    addTorrent task.torrentUrl Nothing (Just tags)
    Log.logInfo_ $ "Added upgrade torrent: " <> toText newEp.episodeNumber

  -- Save new episodes to database (upsert will replace old)
  transact $ mapM_ (\(_, newEp, _) -> EpisodeDB.upsertEpisode newEp) tasks

-- | Tag old torrent for deletion by cleanup job
tagOldForDeletion ::
  (Download :> es, Log :> es, IOE :> es) =>
  Text ->
  Eff es ()
tagOldForDeletion infoHash = do
  addTagsToTorrents [infoHash] (TagList [Deletion])
  Log.logInfo_ $ "Tagged for deletion: " <> infoHash
