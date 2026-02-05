-- | Cleanup job for removing old torrents marked for deletion.
--
-- This module provides cleanup functionality for torrents tagged with 'Deletion'.
-- It checks if the newer version is completed before deleting the old one.
module Moe.App.Subscription.Cleanup
  ( runCleanup,
  )
where

import Data.Map.Strict qualified as Map
import Effectful
import Effectful.Concurrent (Concurrent)
import Effectful.Error.Static (Error)
import Effectful.Log (Log)
import Effectful.Log qualified as Log
import Effectful.Sqlite (Sqlite, transact)
import Moe.Domain.Bangumi.Episode (Episode (..))
import Moe.Error (MoeError)
import Moe.Infrastructure.Database.Episode qualified as EpisodeDB
import Moe.Infrastructure.Download.Effect
import Moe.Prelude

-- | Run cleanup task to delete old torrents tagged for deletion.
--
-- For each torrent tagged with 'Deletion':
-- 1. Find the corresponding episode in database
-- 2. Check if the new version's torrent is completed
-- 3. If completed, delete the old torrent and remove its episode record
runCleanup ::
  (Download :> es, Error MoeError :> es, Sqlite :> es, Concurrent :> es, Log :> es, IOE :> es) =>
  Eff es ()
runCleanup = do
  Log.logInfo_ "Starting cleanup task"

  -- Get all torrents tagged for deletion
  deletionTorrents <- getTorrentsWithTag Deletion

  unless (null deletionTorrents) $ do
    -- Get all episodes from database to find newer versions
    let hashesToFind = map (infoHashToText . (.hash)) deletionTorrents
    episodes <- transact $ EpisodeDB.getEpisodesByInfoHashes hashesToFind

    let episodeMap = Map.fromList [(ep.infoHash, ep) | ep <- episodes]

    -- Process each deletion torrent
    forM_ deletionTorrents $ \torrent -> do
      let torrentHash = infoHashToText torrent.hash
      case Map.lookup torrentHash episodeMap of
        Nothing -> do
          -- No episode record found, delete torrent directly
          Log.logInfo_ $ "No episode record for torrent, deleting: " <> torrent.name
          deleteTorrentAndCleanup torrentHash

        Just oldEp -> do
          -- Find newer episode (same bangumi, same episode number, different hash)
          mNewerEp <- transact $ EpisodeDB.getNewerEpisode oldEp.bangumiId oldEp.episodeNumber oldEp.infoHash

          case mNewerEp of
            Nothing -> do
              -- No newer version exists, don't delete
              Log.logInfo_ $ "No newer episode found, skipping: " <> torrent.name

            Just newerEp -> do
              -- Check if newer torrent is completed
              newerTorrents <- getTorrentsByHashes [newerEp.infoHash]
              case listToMaybe newerTorrents of
                Nothing -> do
                  -- Newer torrent not in downloader, skip
                  Log.logInfo_ $ "Newer torrent not in downloader, skipping: " <> torrent.name

                Just newerTorrent ->
                  if isCompleted newerTorrent
                    then do
                      -- Newer is completed, safe to delete old
                      Log.logInfo_ $ "Newer completed, deleting old: " <> torrent.name
                      deleteTorrentAndCleanup torrentHash
                      -- Also delete the old episode record
                      transact $ EpisodeDB.deleteEpisodeByInfoHash oldEp.infoHash
                    else
                      Log.logInfo_ $ "Newer not completed yet, skipping: " <> torrent.name

-- | Delete torrent from downloader
deleteTorrentAndCleanup ::
  (Download :> es, Log :> es, IOE :> es) =>
  Text ->
  Eff es ()
deleteTorrentAndCleanup hash = do
  -- Delete torrent with files
  deleteTorrents [hash] True
  Log.logInfo_ $ "Deleted torrent: " <> hash
