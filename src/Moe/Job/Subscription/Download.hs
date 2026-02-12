-- | Download helpers for subscription job.
module Moe.Job.Subscription.Download
  ( downloadAndSaveEpisodes,
    deleteReplacedTorrents,
  )
where

import Effectful
import Effectful.Sqlite (transact)
import Moe.Domain.Bangumi (Bangumi)
import Moe.Domain.Episode (Episode (..))
import Moe.Domain.File (generateBaseName, generatePath, toBangumiFile)
import Moe.Domain.Shared.Entity (Entity (..))
import Moe.Infra.Database.Episode qualified as EpisodeDB
import Moe.Infra.Downloader.Effect
import Moe.Prelude

-- | Download torrents and save episodes to database.
downloadAndSaveEpisodes ::
  (Downloader :> es, Sqlite :> es, Concurrent :> es, Log :> es,  IOE :> es) =>
  Entity Bangumi ->
  [Episode] ->
  Eff es ()
downloadAndSaveEpisodes _ [] = pass
downloadAndSaveEpisodes bangumi episodes = do
  transact $ mapM_ EpisodeDB.upsertEpisode episodes
  forM_ episodes $ \ep -> do
    let file = toBangumiFile bangumi.entityVal ep
        params =
          AddTorrentParams
            { url = ep.torrentUrl,
              savePath = Just $ toText $ generatePath file,
              rename = Just $ toText $ generateBaseName file,
              tags = Just [subscriptionTag]
            }
    addTorrent params

-- | Delete old torrents that were replaced by upgrades.
deleteReplacedTorrents ::
  (Downloader :> es, Log :> es, IOE :> es) =>
  [Entity Episode] ->
  Eff es ()
deleteReplacedTorrents [] = pass
deleteReplacedTorrents episodes = do
  let hashes = map (\e -> e.entityVal.infoHash) episodes
  deleteTorrents hashes True
