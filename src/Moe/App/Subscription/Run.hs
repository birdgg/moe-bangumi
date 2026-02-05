module Moe.App.Subscription.Run
  ( runSubscription,
    module Moe.App.Subscription.Types,
  )
where

import Data.Map.Strict qualified as Map
import Effectful
import Effectful.Concurrent (Concurrent)
import Effectful.Error.Static (Error)
import Effectful.Log (Log)
import Effectful.Log qualified as Log
import Effectful.Sqlite (Sqlite, transact)
import Moe.App.Subscription.Fetch (fetchAll)
import Moe.App.Subscription.Filter (filterItems)
import Moe.App.Subscription.Types
import Moe.App.Subscription.Washing (WashingResult (..), buildEpisodeMap, parseFilteredItem, processWashing)
import Moe.Domain.Bangumi.Episode (Episode (..))
import Moe.Domain.Bangumi.Types (Bangumi (..))
import Moe.Domain.Setting.Types (UserPreference (..))
import Moe.Error (MoeError)
import Moe.Infrastructure.Database.Episode qualified as EpisodeDB
import Moe.Infrastructure.Download.Effect
import Moe.Infrastructure.Rss.Effect (Rss)
import Moe.Infrastructure.Setting.Effect (Setting, getSetting)
import Moe.Prelude
import Network.HTTP.Client (Manager)

runSubscription ::
  (Rss :> es, Download :> es, Error MoeError :> es, Setting :> es, Sqlite :> es, Concurrent :> es, Log :> es, IOE :> es) =>
  Manager ->
  Eff es ()
runSubscription manager = do
  Log.logInfo_ "Starting RSS subscription"

  fetchResults <- fetchAll manager
  pref <- getSetting
  washingResults <- concat <$> mapM (processFetchResult pref) fetchResults

  let (newTasks, upgradeTasks) = partitionResults washingResults
  processNewEpisodes newTasks
  processUpgrades upgradeTasks

-- | Process a single FetchResult: filter items and run washing within one RSS URL scope
processFetchResult ::
  (Sqlite :> es, Concurrent :> es, Log :> es, IOE :> es) =>
  UserPreference ->
  FetchResult ->
  Eff es [WashingResult]
processFetchResult pref fr = transact $ do
  let parsedItems = mapMaybe parseFilteredItem $ filterItems pref.filter fr
  case fr.bangumi.id of
    Nothing -> pure []
    Just bid -> do
      episodes <- EpisodeDB.listEpisodesByBangumi bid
      let episodeMap = buildEpisodeMap episodes
      pure $ map (processWashing episodeMap pref.washing) parsedItems

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
