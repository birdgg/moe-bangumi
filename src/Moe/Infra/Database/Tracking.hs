module Moe.Infra.Database.Tracking
  ( getTracking,
    getTrackingByBangumi,
    listTracking,
    listTrackingWithBangumi,
    listEnabledRssTracking,
    listEnabledRssTrackingWithBangumi,
    listEnabledRssTrackingWithBangumiByWeekday,
    createTracking,
    updateTracking,
    deleteTracking,
    upsertTracking,
  )
where

import Effectful
import Effectful.Exception (throwIO)
import Effectful.Sqlite (FromRow (..), Only (..), SqliteTransaction, execute, query, query_)
import Moe.Domain.Bangumi qualified as Bangumi
import Moe.Domain.Shared.Entity (Entity (..), Id (..))
import Moe.Domain.Tracking qualified as Types
import Moe.Error (AppError (..))
import Moe.Infra.Database.Types (DatabaseExecError (..))
import Moe.Prelude

-- | Helper for reading Entity Tracking + Entity Bangumi from JOIN queries.
newtype TrackingBangumiRow = TrackingBangumiRow {unRow :: (Entity Types.Tracking, Entity Bangumi.Bangumi)}

instance FromRow TrackingBangumiRow where
  fromRow = TrackingBangumiRow <$> ((,) <$> fromRow <*> fromRow)

trackingColumns :: Text
trackingColumns = "id, bangumi_id, tracking_type, rss_url, rss_enabled, last_pubdate, current_episode, episode_offset, is_bdrip, auto_complete, created_at, updated_at"

getTracking ::
  (SqliteTransaction :> es, IOE :> es) =>
  Types.TrackingId ->
  Eff es (Maybe (Entity Types.Tracking))
getTracking (Id tid) = do
  results <- query (fromString $ "SELECT " <> toString trackingColumns <> " FROM tracking WHERE id = ?") (Only tid)
  pure $ listToMaybe results

getTrackingByBangumi ::
  (SqliteTransaction :> es, IOE :> es) =>
  Bangumi.BangumiId ->
  Eff es (Maybe (Entity Types.Tracking))
getTrackingByBangumi (Id bid) = do
  results <- query (fromString $ "SELECT " <> toString trackingColumns <> " FROM tracking WHERE bangumi_id = ?") (Only bid)
  pure $ listToMaybe results

listTracking ::
  (SqliteTransaction :> es, IOE :> es) =>
  Eff es [Entity Types.Tracking]
listTracking =
  query_ (fromString $ "SELECT " <> toString trackingColumns <> " FROM tracking")

listTrackingWithBangumi ::
  (SqliteTransaction :> es, IOE :> es) =>
  Eff es [(Entity Types.Tracking, Entity Bangumi.Bangumi)]
listTrackingWithBangumi = do
  rows <-
    query_
      "SELECT \
      \t.id, t.bangumi_id, t.tracking_type, t.rss_url, t.rss_enabled, t.last_pubdate, t.current_episode, t.episode_offset, t.is_bdrip, t.auto_complete, t.created_at, t.updated_at, \
      \b.id, b.title_chs, b.title_jap, b.air_date, b.season, b.kind, b.mikan_id, b.tmdb_id, b.bgmtv_id, b.poster_url, b.total_episodes, b.created_at, b.updated_at \
      \FROM tracking t \
      \INNER JOIN bangumi b ON t.bangumi_id = b.id"
  pure $ map unRow rows

listEnabledRssTracking ::
  (SqliteTransaction :> es, IOE :> es) =>
  Eff es [Entity Types.Tracking]
listEnabledRssTracking =
  query_
    (fromString $ "SELECT " <> toString trackingColumns <> " FROM tracking WHERE rss_enabled = 1 AND rss_url IS NOT NULL")

-- | List enabled RSS trackings with their associated bangumi (JOIN query)
listEnabledRssTrackingWithBangumi ::
  (SqliteTransaction :> es, IOE :> es) =>
  Eff es [(Entity Types.Tracking, Entity Bangumi.Bangumi)]
listEnabledRssTrackingWithBangumi = do
  rows <-
    query_
      "SELECT \
      \t.id, t.bangumi_id, t.tracking_type, t.rss_url, t.rss_enabled, t.last_pubdate, t.current_episode, t.episode_offset, t.is_bdrip, t.auto_complete, t.created_at, t.updated_at, \
      \b.id, b.title_chs, b.title_jap, b.air_date, b.season, b.kind, b.mikan_id, b.tmdb_id, b.bgmtv_id, b.poster_url, b.total_episodes, b.created_at, b.updated_at \
      \FROM tracking t \
      \INNER JOIN bangumi b ON t.bangumi_id = b.id \
      \WHERE t.rss_enabled = 1 AND t.rss_url IS NOT NULL"
  pure $ map unRow rows

-- | List enabled RSS trackings filtered by bangumi air weekday.
-- Weekday uses SQLite format: 0=Sunday, 1=Monday, ..., 6=Saturday.
listEnabledRssTrackingWithBangumiByWeekday ::
  (SqliteTransaction :> es, IOE :> es) =>
  Int ->
  Eff es [(Entity Types.Tracking, Entity Bangumi.Bangumi)]
listEnabledRssTrackingWithBangumiByWeekday weekday = do
  rows <-
    query
      "SELECT \
      \t.id, t.bangumi_id, t.tracking_type, t.rss_url, t.rss_enabled, t.last_pubdate, t.current_episode, t.episode_offset, t.is_bdrip, t.auto_complete, t.created_at, t.updated_at, \
      \b.id, b.title_chs, b.title_jap, b.air_date, b.season, b.kind, b.mikan_id, b.tmdb_id, b.bgmtv_id, b.poster_url, b.total_episodes, b.created_at, b.updated_at \
      \FROM tracking t \
      \INNER JOIN bangumi b ON t.bangumi_id = b.id \
      \WHERE t.rss_enabled = 1 AND t.rss_url IS NOT NULL \
      \AND CAST(strftime('%w', b.air_date) AS INTEGER) = ?"
      (Only weekday)
  pure $ map unRow rows

createTracking ::
  (SqliteTransaction :> es, IOE :> es) =>
  Types.Tracking ->
  Eff es Types.TrackingId
createTracking tracking = do
  results <-
    query
      "INSERT INTO tracking (bangumi_id, tracking_type, rss_url, rss_enabled, last_pubdate, current_episode, episode_offset, is_bdrip, auto_complete) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?) RETURNING id"
      ( tracking.bangumiId,
        tracking.trackingType,
        tracking.rssUrl,
        tracking.rssEnabled,
        tracking.lastPubdate,
        tracking.currentEpisode,
        tracking.episodeOffset,
        tracking.isBDrip,
        tracking.autoComplete
      )
  case results of
    [Only tid] -> pure $ Id tid
    _ -> throwIO $ DatabaseError (DbUnexpectedResult "createTracking: unexpected result")

updateTracking ::
  (SqliteTransaction :> es, IOE :> es) =>
  Entity Types.Tracking ->
  Eff es ()
updateTracking entity =
  let Id tid = entity.entityId
      t = entity.entityVal
   in execute
        "UPDATE tracking SET bangumi_id = ?, tracking_type = ?, rss_url = ?, rss_enabled = ?, last_pubdate = ?, current_episode = ?, episode_offset = ?, is_bdrip = ?, auto_complete = ?, updated_at = datetime('now') WHERE id = ?"
        ( t.bangumiId,
          t.trackingType,
          t.rssUrl,
          t.rssEnabled,
          t.lastPubdate,
          t.currentEpisode,
          t.episodeOffset,
          t.isBDrip,
          t.autoComplete,
          tid
        )

upsertTracking ::
  (SqliteTransaction :> es, IOE :> es) =>
  Types.Tracking ->
  Eff es Types.TrackingId
upsertTracking tracking = do
  results <-
    query
      "INSERT INTO tracking (bangumi_id, tracking_type, rss_url, rss_enabled, last_pubdate, current_episode, episode_offset, is_bdrip, auto_complete) \
      \VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?) \
      \ON CONFLICT (bangumi_id) DO UPDATE SET \
      \tracking_type = excluded.tracking_type, \
      \rss_url = excluded.rss_url, \
      \rss_enabled = excluded.rss_enabled, \
      \last_pubdate = excluded.last_pubdate, \
      \current_episode = MAX(tracking.current_episode, excluded.current_episode), \
      \episode_offset = excluded.episode_offset, \
      \is_bdrip = excluded.is_bdrip, \
      \auto_complete = excluded.auto_complete, \
      \updated_at = datetime('now') \
      \RETURNING id"
      ( tracking.bangumiId,
        tracking.trackingType,
        tracking.rssUrl,
        tracking.rssEnabled,
        tracking.lastPubdate,
        tracking.currentEpisode,
        tracking.episodeOffset,
        tracking.isBDrip,
        tracking.autoComplete
      )
  case results of
    [Only tid] -> pure $ Id tid
    _ -> throwIO $ DatabaseError (DbUnexpectedResult "upsertTracking: unexpected result")

deleteTracking ::
  (SqliteTransaction :> es, IOE :> es) =>
  Types.TrackingId ->
  Eff es ()
deleteTracking (Id tid) =
  execute "DELETE FROM tracking WHERE id = ?" (Only tid)
