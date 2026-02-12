module Moe.Infra.Database.Episode
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
import Effectful.Exception (throwIO)
import Effectful.Sqlite (Only (..), SqliteTransaction, execute, query)
import Moe.Domain.Episode (Episode (..), EpisodeId, EpisodeNumber)
import Moe.Domain.Bangumi (BangumiId)
import Moe.Domain.Shared.Entity (Entity, Id (..))
import Moe.Error (AppError (..))
import Moe.Infra.Database.Types (DatabaseExecError (..))
import Moe.Prelude

episodeColumns :: Text
episodeColumns = "id, bangumi_id, episode_number, \"group\", subtitle_list, resolution, info_hash, torrent_url, pub_date, created_at, updated_at"

getEpisode ::
  (SqliteTransaction :> es, IOE :> es) =>
  BangumiId ->
  EpisodeNumber ->
  Eff es (Maybe (Entity Episode))
getEpisode (Id bid) epNum = do
  results <-
    query
      (fromString $ "SELECT " <> toString episodeColumns <> " FROM episode WHERE bangumi_id = ? AND episode_number = ?")
      (bid, epNum)
  pure $ listToMaybe results

listEpisodesByBangumi ::
  (SqliteTransaction :> es, IOE :> es) =>
  BangumiId ->
  Eff es [Entity Episode]
listEpisodesByBangumi (Id bid) =
  query
    (fromString $ "SELECT " <> toString episodeColumns <> " FROM episode WHERE bangumi_id = ?")
    (Only bid)

upsertEpisode ::
  (SqliteTransaction :> es, IOE :> es) =>
  Episode ->
  Eff es EpisodeId
upsertEpisode ep = do
  results <-
    query
      "INSERT INTO episode (bangumi_id, episode_number, \"group\", subtitle_list, resolution, info_hash, torrent_url, pub_date) \
      \VALUES (?, ?, ?, ?, ?, ?, ?, ?) \
      \ON CONFLICT (bangumi_id, episode_number) DO UPDATE SET \
      \\"group\" = excluded.\"group\", \
      \subtitle_list = excluded.subtitle_list, \
      \resolution = excluded.resolution, \
      \info_hash = excluded.info_hash, \
      \torrent_url = excluded.torrent_url, \
      \pub_date = excluded.pub_date, \
      \updated_at = datetime('now') \
      \RETURNING id"
      ( ep.bangumiId,
        ep.episodeNumber,
        ep.group,
        ep.subtitleList,
        ep.resolution,
        ep.infoHash,
        ep.torrentUrl,
        ep.pubDate
      )
  case results of
    [Only eid] -> pure $ Id eid
    _ -> throwIO $ DatabaseError (DbUnexpectedResult "upsertEpisode: unexpected result")

deleteEpisode ::
  (SqliteTransaction :> es, IOE :> es) =>
  EpisodeId ->
  Eff es ()
deleteEpisode (Id eid) =
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
  Eff es [Entity Episode]
getEpisodesByInfoHashes [] = pure []
getEpisodesByInfoHashes hashes = do
  let placeholders = T.intercalate "," $ replicate (length hashes) "?"
      sql = "SELECT " <> episodeColumns <> " FROM episode WHERE info_hash IN (" <> placeholders <> ")"
  query (fromString $ toString sql) hashes

-- | Get newer episode (same bangumi, same episode number, different hash, newer pub_date)
getNewerEpisode ::
  (SqliteTransaction :> es, IOE :> es) =>
  BangumiId ->
  EpisodeNumber ->
  Text ->
  Eff es (Maybe (Entity Episode))
getNewerEpisode (Id bid) epNum oldHash = do
  results <-
    query
      (fromString $ "SELECT " <> toString episodeColumns <> " FROM episode WHERE bangumi_id = ? AND episode_number = ? AND info_hash != ? ORDER BY pub_date DESC LIMIT 1")
      (bid, epNum, oldHash)
  pure $ listToMaybe results
