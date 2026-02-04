module Moe.Infrastructure.Database.Tracking
  ( getTracking,
    getTrackingByBangumi,
    listTracking,
    listTrackingWithBangumi,
    listEnabledRssTracking,
    createTracking,
    updateTracking,
    deleteTracking,
    upsertTracking,
  )
where

import Data.Text qualified as T
import Effectful
import Effectful.Sqlite (FromRow (..), Only (..), SqliteTransaction, execute, field, query, query_)
import Moe.Domain.Bangumi.Types qualified as Bangumi
import Moe.Domain.Tracking.Types qualified as Types
import Moe.Infrastructure.Database.Orphans ()
import Moe.Prelude

newtype TrackingBangumiRow = TrackingBangumiRow {unRow :: (Types.Tracking, Bangumi.Bangumi)}

instance FromRow TrackingBangumiRow where
  fromRow = do
    tracking <-
      Types.Tracking
        <$> field
        <*> field
        <*> field
        <*> field
        <*> field
        <*> field
        <*> field
        <*> field
    bangumi <-
      Bangumi.Bangumi
        <$> field
        <*> field
        <*> field
        <*> field
        <*> field
        <*> field
        <*> field
        <*> field
        <*> field
        <*> field
    pure $ TrackingBangumiRow (tracking, bangumi)

trackingColumns :: Text
trackingColumns = "id, bangumi_id, tracking_type, rss_url, rss_enabled, last_pubdate, current_episode, created_at"

getTracking ::
  (SqliteTransaction :> es, IOE :> es) =>
  Types.TrackingId ->
  Eff es (Maybe Types.Tracking)
getTracking (Types.TrackingId tid) = do
  results <- query (fromString $ "SELECT " <> T.unpack trackingColumns <> " FROM tracking WHERE id = ?") (Only tid)
  pure $ listToMaybe results

getTrackingByBangumi ::
  (SqliteTransaction :> es, IOE :> es) =>
  Bangumi.BangumiId ->
  Eff es (Maybe Types.Tracking)
getTrackingByBangumi (Bangumi.BangumiId bid) = do
  results <- query (fromString $ "SELECT " <> T.unpack trackingColumns <> " FROM tracking WHERE bangumi_id = ?") (Only bid)
  pure $ listToMaybe results

listTracking ::
  (SqliteTransaction :> es, IOE :> es) =>
  Eff es [Types.Tracking]
listTracking =
  query_ (fromString $ "SELECT " <> T.unpack trackingColumns <> " FROM tracking")

listTrackingWithBangumi ::
  (SqliteTransaction :> es, IOE :> es) =>
  Eff es [(Types.Tracking, Bangumi.Bangumi)]
listTrackingWithBangumi = do
  rows <-
    query_
      "SELECT \
      \t.id, t.bangumi_id, t.tracking_type, t.rss_url, t.rss_enabled, t.last_pubdate, t.current_episode, t.created_at, \
      \b.id, b.title_chs, b.title_jap, b.air_date, b.season_number, b.kind, b.mikan_id, b.tmdb_id, b.bangumi_tv_id, b.poster_url \
      \FROM tracking t \
      \INNER JOIN bangumi b ON t.bangumi_id = b.id"
  pure $ map unRow rows

listEnabledRssTracking ::
  (SqliteTransaction :> es, IOE :> es) =>
  Eff es [Types.Tracking]
listEnabledRssTracking =
  query_
    (fromString $ "SELECT " <> T.unpack trackingColumns <> " FROM tracking WHERE rss_enabled = 1 AND rss_url IS NOT NULL")

createTracking ::
  (SqliteTransaction :> es, IOE :> es) =>
  Types.Tracking ->
  Eff es Types.TrackingId
createTracking tracking = do
  results <-
    query
      "INSERT INTO tracking (bangumi_id, tracking_type, rss_url, rss_enabled, last_pubdate, current_episode) VALUES (?, ?, ?, ?, ?, ?) RETURNING id"
      ( tracking.bangumiId,
        tracking.trackingType,
        tracking.rssUrl,
        tracking.rssEnabled,
        tracking.lastPubdate,
        tracking.currentEpisode
      )
  case results of
    [Only tid] -> pure $ Types.TrackingId tid
    _ -> error "createTracking: unexpected result"

updateTracking ::
  (SqliteTransaction :> es, IOE :> es) =>
  Types.Tracking ->
  Eff es ()
updateTracking tracking =
  case tracking.id of
    Nothing -> pure ()
    Just (Types.TrackingId tid) ->
      execute
        "UPDATE tracking SET bangumi_id = ?, tracking_type = ?, rss_url = ?, rss_enabled = ?, last_pubdate = ?, current_episode = ? WHERE id = ?"
        ( tracking.bangumiId,
          tracking.trackingType,
          tracking.rssUrl,
          tracking.rssEnabled,
          tracking.lastPubdate,
          tracking.currentEpisode,
          tid
        )

upsertTracking ::
  (SqliteTransaction :> es, IOE :> es) =>
  Types.Tracking ->
  Eff es Types.TrackingId
upsertTracking tracking = do
  results <-
    query
      "INSERT INTO tracking (bangumi_id, tracking_type, rss_url, rss_enabled, last_pubdate, current_episode) \
      \VALUES (?, ?, ?, ?, ?, ?) \
      \ON CONFLICT (bangumi_id) DO UPDATE SET \
      \tracking_type = excluded.tracking_type, \
      \rss_url = excluded.rss_url, \
      \rss_enabled = excluded.rss_enabled, \
      \last_pubdate = excluded.last_pubdate, \
      \current_episode = excluded.current_episode \
      \RETURNING id"
      ( tracking.bangumiId,
        tracking.trackingType,
        tracking.rssUrl,
        tracking.rssEnabled,
        tracking.lastPubdate,
        tracking.currentEpisode
      )
  case results of
    [Only tid] -> pure $ Types.TrackingId tid
    _ -> error "upsertTracking: unexpected result"

deleteTracking ::
  (SqliteTransaction :> es, IOE :> es) =>
  Types.TrackingId ->
  Eff es ()
deleteTracking (Types.TrackingId tid) =
  execute "DELETE FROM tracking WHERE id = ?" (Only tid)
