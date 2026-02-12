module Moe.Infra.Database.Bangumi
  ( getBangumi,
    getBangumiByIds,
    findBangumiByTmdbId,
    listBangumi,
    listBangumiBySeason,
    createBangumi,
    updateBangumi,
    deleteBangumi,
    upsertBangumi,
  )
where

import Data.Time (UTCTime)
import Effectful
import Effectful.Exception (throwIO)
import Effectful.Sqlite (Only (..), (:.) (..), SqliteTransaction, execute, query, query_)
import Moe.Domain.Bangumi qualified as Types
import Moe.Domain.Shared.Entity (Entity (..), Id (..))
import Moe.Domain.Shared.Metadata (TmdbId)
import Moe.Error (AppError (..))
import Moe.Infra.Database.Types (DatabaseExecError (..))
import Moe.Prelude

bangumiColumns :: Text
bangumiColumns = "id, title_chs, title_jap, air_date, season, kind, mikan_id, tmdb_id, bgmtv_id, poster_url, total_episodes, created_at, updated_at"

getBangumi ::
  (SqliteTransaction :> es, IOE :> es) =>
  Types.BangumiId ->
  Eff es (Maybe (Entity Types.Bangumi))
getBangumi (Id bid) = do
  results <- query (fromString $ "SELECT " <> toString bangumiColumns <> " FROM bangumi WHERE id = ?") (Only bid)
  pure $ listToMaybe results

getBangumiByIds ::
  (SqliteTransaction :> es, IOE :> es) =>
  [Types.BangumiId] ->
  Eff es [Entity Types.Bangumi]
getBangumiByIds [] = pure []
getBangumiByIds bids = do
  let ids = map (\(Id i) -> i) bids
      placeholders = intercalate ", " (replicate (length ids) "?")
      sql = "SELECT " <> toString bangumiColumns <> " FROM bangumi WHERE id IN (" <> placeholders <> ")"
  query (fromString sql) ids

-- | Find a bangumi by its TMDB ID.
findBangumiByTmdbId ::
  (SqliteTransaction :> es, IOE :> es) =>
  TmdbId ->
  Eff es (Maybe (Entity Types.Bangumi))
findBangumiByTmdbId tmdbId = do
  results <- query (fromString $ "SELECT " <> toString bangumiColumns <> " FROM bangumi WHERE tmdb_id = ?") (Only tmdbId)
  pure $ listToMaybe results

listBangumi ::
  (SqliteTransaction :> es, IOE :> es) =>
  Eff es [Entity Types.Bangumi]
listBangumi =
  query_ (fromString $ "SELECT " <> toString bangumiColumns <> " FROM bangumi")

listBangumiBySeason ::
  (SqliteTransaction :> es, IOE :> es) =>
  Types.AirSeason ->
  Eff es [Entity Types.Bangumi]
listBangumiBySeason (Types.AirSeason year season) = do
  let months = Types.seasonToMonths season
      monthPlaceholders = intercalate ", " (replicate (length months) "?")
      sql =
        "SELECT "
          <> toString bangumiColumns
          <> " FROM bangumi WHERE strftime('%Y', air_date) = ? AND CAST(strftime('%m', air_date) AS INTEGER) IN ("
          <> monthPlaceholders
          <> ")"
      params :: [String] = show year : map show months
  query (fromString sql) params

createBangumi ::
  (SqliteTransaction :> es, IOE :> es) =>
  Types.Bangumi ->
  Eff es (Types.BangumiId, UTCTime, UTCTime)
createBangumi bangumi = do
  results <-
    query
      "INSERT INTO bangumi (title_chs, title_jap, air_date, season, kind, mikan_id, tmdb_id, bgmtv_id, poster_url, total_episodes) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?) RETURNING id, created_at, updated_at"
      ( bangumi.titleChs,
        bangumi.titleJap,
        bangumi.airDate,
        bangumi.season,
        bangumi.kind,
        bangumi.mikanId,
        bangumi.tmdbId,
        bangumi.bgmtvId,
        bangumi.posterUrl,
        bangumi.totalEpisodes
      )
  case results of
    [(bid, ts, uts)] -> pure (Id bid, ts, uts)
    _ -> throwIO $ DatabaseError (DbUnexpectedResult "createBangumi: unexpected RETURNING result")

updateBangumi ::
  (SqliteTransaction :> es, IOE :> es) =>
  Entity Types.Bangumi ->
  Eff es ()
updateBangumi entity =
  let Id bid = entity.entityId
      b = entity.entityVal
   in execute
        "UPDATE bangumi SET title_chs = ?, title_jap = ?, air_date = ?, season = ?, kind = ?, mikan_id = ?, tmdb_id = ?, bgmtv_id = ?, poster_url = ?, total_episodes = ?, updated_at = datetime('now') WHERE id = ?"
        ( ( b.titleChs,
            b.titleJap,
            b.airDate,
            b.season,
            b.kind,
            b.mikanId,
            b.tmdbId,
            b.bgmtvId,
            b.posterUrl,
            b.totalEpisodes
          )
            :. Only bid
        )

-- | Upsert a bangumi, deduplicating by bgmtv_id, tmdb_id + air_date, or title_chs + air_date.
--
-- SQLite treats NULL as distinct in UNIQUE constraints, so ON CONFLICT (bgmtv_id)
-- does not trigger when bgmtv_id is NULL. This function falls back to application-level
-- lookups when bgmtv_id is absent.
upsertBangumi ::
  (SqliteTransaction :> es, IOE :> es) =>
  Types.Bangumi ->
  Eff es (Types.BangumiId, UTCTime, UTCTime)
upsertBangumi bangumi = case bangumi.bgmtvId of
  Just _ -> upsertByBgmtvId bangumi
  Nothing -> do
    existing <- findExistingBangumi bangumi
    case existing of
      Just entity -> do
        updateBangumi entity {entityVal = bangumi}
        pure (entity.entityId, entity.createdAt, entity.updatedAt)
      Nothing -> createBangumi bangumi

-- | Find an existing bangumi by tmdb_id + air_date, falling back to title_chs + air_date.
findExistingBangumi ::
  (SqliteTransaction :> es, IOE :> es) =>
  Types.Bangumi ->
  Eff es (Maybe (Entity Types.Bangumi))
findExistingBangumi bangumi = runMaybeT $ byTmdbIdAndAirDate <|> byTitleAndAirDate
  where
    byTmdbIdAndAirDate = do
      tid <- MaybeT $ pure bangumi.tmdbId
      MaybeT $ findBangumiByTmdbIdAndAirDate tid bangumi.airDate
    byTitleAndAirDate =
      MaybeT $ findBangumiByTitleAndAirDate bangumi.titleChs bangumi.airDate

findBangumiByTmdbIdAndAirDate ::
  (SqliteTransaction :> es, IOE :> es) =>
  TmdbId ->
  Types.AirDate ->
  Eff es (Maybe (Entity Types.Bangumi))
findBangumiByTmdbIdAndAirDate tmdbId airDate = do
  results <- query (fromString $ "SELECT " <> toString bangumiColumns <> " FROM bangumi WHERE tmdb_id = ? AND air_date = ?") (tmdbId, airDate)
  pure $ listToMaybe results

findBangumiByTitleAndAirDate ::
  (SqliteTransaction :> es, IOE :> es) =>
  Text ->
  Types.AirDate ->
  Eff es (Maybe (Entity Types.Bangumi))
findBangumiByTitleAndAirDate titleChs airDate = do
  results <- query (fromString $ "SELECT " <> toString bangumiColumns <> " FROM bangumi WHERE title_chs = ? AND air_date = ?") (titleChs, airDate)
  pure $ listToMaybe results

-- | Upsert via SQL ON CONFLICT (bgmtv_id). Only valid when bgmtv_id is NOT NULL.
upsertByBgmtvId ::
  (SqliteTransaction :> es, IOE :> es) =>
  Types.Bangumi ->
  Eff es (Types.BangumiId, UTCTime, UTCTime)
upsertByBgmtvId bangumi = do
  results <-
    query
      "INSERT INTO bangumi (title_chs, title_jap, air_date, season, kind, mikan_id, tmdb_id, bgmtv_id, poster_url, total_episodes) \
      \VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?) \
      \ON CONFLICT (bgmtv_id) DO UPDATE SET \
      \title_chs = excluded.title_chs, \
      \title_jap = excluded.title_jap, \
      \air_date = excluded.air_date, \
      \season = excluded.season, \
      \kind = excluded.kind, \
      \mikan_id = excluded.mikan_id, \
      \tmdb_id = excluded.tmdb_id, \
      \poster_url = excluded.poster_url, \
      \total_episodes = excluded.total_episodes, \
      \updated_at = datetime('now') \
      \RETURNING id, created_at, updated_at"
      ( bangumi.titleChs,
        bangumi.titleJap,
        bangumi.airDate,
        bangumi.season,
        bangumi.kind,
        bangumi.mikanId,
        bangumi.tmdbId,
        bangumi.bgmtvId,
        bangumi.posterUrl,
        bangumi.totalEpisodes
      )
  case results of
    [(bid, ts, uts)] -> pure (Id bid, ts, uts)
    _ -> throwIO $ DatabaseError (DbUnexpectedResult "upsertByBgmtvId: unexpected RETURNING result")

deleteBangumi ::
  (SqliteTransaction :> es, IOE :> es) =>
  Types.BangumiId ->
  Eff es ()
deleteBangumi (Id bid) =
  execute "DELETE FROM bangumi WHERE id = ?" (Only bid)
