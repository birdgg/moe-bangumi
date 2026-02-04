module Moe.Infrastructure.Database.Episode
  ( getEpisode,
    listEpisodesByBangumi,
    upsertEpisode,
    deleteEpisode,
    deleteEpisodeByInfoHash,
    getEpisodesByInfoHashes,
    getNewerEpisode,
  )
where

import Data.Text qualified as T
import Effectful
import Effectful.Sqlite (Only (..), SqliteTransaction, execute, query)
import Moe.Domain.Bangumi.Episode (Episode (..), EpisodeId (..), EpisodeNumber)
import Moe.Domain.Bangumi.Types (BangumiId (..))
import Moe.Infrastructure.Database.Orphans ()
import Moe.Prelude

episodeColumns :: Text
episodeColumns = "id, bangumi_id, episode_number, \"group\", resolution, info_hash, torrent_url, pub_date, created_at"

getEpisode ::
  (SqliteTransaction :> es, IOE :> es) =>
  BangumiId ->
  EpisodeNumber ->
  Eff es (Maybe Episode)
getEpisode (BangumiId bid) epNum = do
  results <-
    query
      (fromString $ "SELECT " <> T.unpack episodeColumns <> " FROM episode WHERE bangumi_id = ? AND episode_number = ?")
      (bid, epNum)
  pure $ listToMaybe results

listEpisodesByBangumi ::
  (SqliteTransaction :> es, IOE :> es) =>
  BangumiId ->
  Eff es [Episode]
listEpisodesByBangumi (BangumiId bid) =
  query
    (fromString $ "SELECT " <> T.unpack episodeColumns <> " FROM episode WHERE bangumi_id = ?")
    (Only bid)

upsertEpisode ::
  (SqliteTransaction :> es, IOE :> es) =>
  Episode ->
  Eff es EpisodeId
upsertEpisode ep = do
  results <-
    query
      "INSERT INTO episode (bangumi_id, episode_number, \"group\", resolution, info_hash, torrent_url, pub_date) \
      \VALUES (?, ?, ?, ?, ?, ?, ?) \
      \ON CONFLICT (bangumi_id, episode_number) DO UPDATE SET \
      \\"group\" = excluded.\"group\", \
      \resolution = excluded.resolution, \
      \info_hash = excluded.info_hash, \
      \torrent_url = excluded.torrent_url, \
      \pub_date = excluded.pub_date \
      \RETURNING id"
      ( ep.bangumiId,
        ep.episodeNumber,
        ep.group,
        ep.resolution,
        ep.infoHash,
        ep.torrentUrl,
        ep.pubDate
      )
  case results of
    [Only eid] -> pure $ EpisodeId eid
    _ -> error "upsertEpisode: unexpected result"

deleteEpisode ::
  (SqliteTransaction :> es, IOE :> es) =>
  EpisodeId ->
  Eff es ()
deleteEpisode (EpisodeId eid) =
  execute "DELETE FROM episode WHERE id = ?" (Only eid)

-- | Delete episode by info hash
deleteEpisodeByInfoHash ::
  (SqliteTransaction :> es, IOE :> es) =>
  Text ->
  Eff es ()
deleteEpisodeByInfoHash infoHash =
  execute "DELETE FROM episode WHERE info_hash = ?" (Only infoHash)

-- | Get episodes by info hashes
getEpisodesByInfoHashes ::
  (SqliteTransaction :> es, IOE :> es) =>
  [Text] ->
  Eff es [Episode]
getEpisodesByInfoHashes [] = pure []
getEpisodesByInfoHashes hashes = do
  let placeholders = T.intercalate "," $ replicate (length hashes) "?"
      sql = "SELECT " <> episodeColumns <> " FROM episode WHERE info_hash IN (" <> placeholders <> ")"
  query (fromString $ T.unpack sql) hashes

-- | Get newer episode (same bangumi, same episode number, different hash, newer pub_date)
getNewerEpisode ::
  (SqliteTransaction :> es, IOE :> es) =>
  BangumiId ->
  EpisodeNumber ->
  Text ->
  Eff es (Maybe Episode)
getNewerEpisode (BangumiId bid) epNum oldHash = do
  results <-
    query
      (fromString $ "SELECT " <> T.unpack episodeColumns <> " FROM episode WHERE bangumi_id = ? AND episode_number = ? AND info_hash != ? ORDER BY pub_date DESC LIMIT 1")
      (bid, epNum, oldHash)
  pure $ listToMaybe results
