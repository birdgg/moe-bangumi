module Moe.Adapter.Database.Tracking
  ( getTracking,
    getTrackingByBangumi,
    listTracking,
    createTracking,
    updateTracking,
    deleteTracking,
    upsertTracking,
  )
where

import Data.Maybe (listToMaybe)
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as T
import Effectful
import Effectful.Sqlite (Only (..), SqliteTransaction, execute, query, query_)
import Moe.Adapter.Database.Orphans ()
import Moe.Domain.Bangumi.Types qualified as Bangumi
import Moe.Domain.Tracking.Types qualified as Types

trackingColumns :: Text
trackingColumns = "id, bangumi_id, tracking_type, rss_url, last_pubdate, current_episode, created_at"

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

createTracking ::
  (SqliteTransaction :> es, IOE :> es) =>
  Types.Tracking ->
  Eff es Types.TrackingId
createTracking tracking = do
  results <-
    query
      "INSERT INTO tracking (bangumi_id, tracking_type, rss_url, last_pubdate, current_episode) VALUES (?, ?, ?, ?, ?) RETURNING id"
      ( tracking.bangumiId,
        tracking.trackingType,
        tracking.rssUrl,
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
        "UPDATE tracking SET bangumi_id = ?, tracking_type = ?, rss_url = ?, last_pubdate = ?, current_episode = ? WHERE id = ?"
        ( tracking.bangumiId,
          tracking.trackingType,
          tracking.rssUrl,
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
      "INSERT INTO tracking (bangumi_id, tracking_type, rss_url, last_pubdate, current_episode) \
      \VALUES (?, ?, ?, ?, ?) \
      \ON CONFLICT (bangumi_id) DO UPDATE SET \
      \tracking_type = excluded.tracking_type, \
      \rss_url = excluded.rss_url, \
      \last_pubdate = excluded.last_pubdate, \
      \current_episode = excluded.current_episode \
      \RETURNING id"
      ( tracking.bangumiId,
        tracking.trackingType,
        tracking.rssUrl,
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
