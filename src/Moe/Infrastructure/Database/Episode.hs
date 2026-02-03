module Moe.Infrastructure.Database.Episode
  ( getEpisode,
    listEpisodesByBangumi,
    upsertEpisode,
    deleteEpisode,
  )
where

import Data.Maybe (listToMaybe)
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as T
import Effectful
import Effectful.Sqlite (Only (..), SqliteTransaction, execute, query)
import Moe.Domain.Bangumi.Types (BangumiId (..))
import Moe.Domain.Bangumi.Episode.Types (Episode (..), EpisodeId (..), EpisodeNumber)
import Moe.Infrastructure.Database.Orphans ()

episodeColumns :: Text
episodeColumns = "id, bangumi_id, episode_number, subtitle_group, resolution, info_hash, torrent_url, pub_date, created_at"

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
      "INSERT INTO episode (bangumi_id, episode_number, subtitle_group, resolution, info_hash, torrent_url, pub_date) \
      \VALUES (?, ?, ?, ?, ?, ?, ?) \
      \ON CONFLICT (bangumi_id, episode_number) DO UPDATE SET \
      \subtitle_group = excluded.subtitle_group, \
      \resolution = excluded.resolution, \
      \info_hash = excluded.info_hash, \
      \torrent_url = excluded.torrent_url, \
      \pub_date = excluded.pub_date \
      \RETURNING id"
      ( ep.bangumiId,
        ep.episodeNumber,
        ep.subtitleGroup,
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
