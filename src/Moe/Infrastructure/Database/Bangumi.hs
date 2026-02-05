module Moe.Infrastructure.Database.Bangumi
  ( getBangumi,
    getBangumiByIds,
    listBangumi,
    listBangumiBySeason,
    createBangumi,
    updateBangumi,
    deleteBangumi,
    upsertBangumi,
  )
where

import Data.Text qualified as T
import Data.Time (UTCTime)
import Effectful
import Effectful.Sqlite (Only (..), SqliteTransaction, execute, query, query_)
import Moe.Domain.Bangumi.Types qualified as Types
import Moe.Infrastructure.Database.Orphans ()
import Moe.Prelude

bangumiColumns :: Text
bangumiColumns = "id, title_chs, title_jap, air_date, season, kind, mikan_id, tmdb_id, bangumi_tv_id, poster_url, created_at"

getBangumi ::
  (SqliteTransaction :> es, IOE :> es) =>
  Types.BangumiId ->
  Eff es (Maybe Types.Bangumi)
getBangumi (Types.BangumiId bid) = do
  results <- query (fromString $ "SELECT " <> T.unpack bangumiColumns <> " FROM bangumi WHERE id = ?") (Only bid)
  pure $ listToMaybe results

getBangumiByIds ::
  (SqliteTransaction :> es, IOE :> es) =>
  [Types.BangumiId] ->
  Eff es [Types.Bangumi]
getBangumiByIds [] = pure []
getBangumiByIds bids = do
  let ids = map (\(Types.BangumiId i) -> i) bids
      placeholders = intercalate ", " (replicate (length ids) "?")
      sql = "SELECT " <> T.unpack bangumiColumns <> " FROM bangumi WHERE id IN (" <> placeholders <> ")"
  query (fromString sql) ids

listBangumi ::
  (SqliteTransaction :> es, IOE :> es) =>
  Eff es [Types.Bangumi]
listBangumi =
  query_ (fromString $ "SELECT " <> T.unpack bangumiColumns <> " FROM bangumi")

listBangumiBySeason ::
  (SqliteTransaction :> es, IOE :> es) =>
  Types.AirSeason ->
  Eff es [Types.Bangumi]
listBangumiBySeason (Types.AirSeason year season) = do
  let months = Types.seasonToMonths season
      monthPlaceholders = intercalate ", " (replicate (length months) "?")
      sql =
        "SELECT "
          <> T.unpack bangumiColumns
          <> " FROM bangumi WHERE strftime('%Y', air_date) = ? AND CAST(strftime('%m', air_date) AS INTEGER) IN ("
          <> monthPlaceholders
          <> ")"
      params :: [String] = show year : map show months
  query (fromString sql) params

createBangumi ::
  (SqliteTransaction :> es, IOE :> es) =>
  Types.NewBangumi ->
  Eff es (Types.BangumiId, UTCTime)
createBangumi bangumi = do
  results <-
    query
      "INSERT INTO bangumi (title_chs, title_jap, air_date, season, kind, mikan_id, tmdb_id, bangumi_tv_id, poster_url) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?) RETURNING id, created_at"
      ( bangumi.titleChs,
        bangumi.titleJap,
        bangumi.airDate,
        bangumi.season,
        bangumi.kind,
        bangumi.mikanId,
        bangumi.tmdbId,
        bangumi.bgmtvId,
        bangumi.posterUrl
      )
  case results of
    [(bid, ts)] -> pure (Types.BangumiId bid, ts)
    _ -> error "createBangumi: unexpected result"

updateBangumi ::
  (SqliteTransaction :> es, IOE :> es) =>
  Types.Bangumi ->
  Eff es ()
updateBangumi bangumi =
  let Types.BangumiId bid = bangumi.id
   in execute
        "UPDATE bangumi SET title_chs = ?, title_jap = ?, air_date = ?, season = ?, kind = ?, mikan_id = ?, tmdb_id = ?, bangumi_tv_id = ?, poster_url = ? WHERE id = ?"
        ( bangumi.titleChs,
          bangumi.titleJap,
          bangumi.airDate,
          bangumi.season,
          bangumi.kind,
          bangumi.mikanId,
          bangumi.tmdbId,
          bangumi.bgmtvId,
          bangumi.posterUrl,
          bid
        )

upsertBangumi ::
  (SqliteTransaction :> es, IOE :> es) =>
  Types.NewBangumi ->
  Eff es (Types.BangumiId, UTCTime)
upsertBangumi bangumi = do
  results <-
    query
      "INSERT INTO bangumi (title_chs, title_jap, air_date, season, kind, mikan_id, tmdb_id, bangumi_tv_id, poster_url) \
      \VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?) \
      \ON CONFLICT (bangumi_tv_id) DO UPDATE SET \
      \title_chs = excluded.title_chs, \
      \title_jap = excluded.title_jap, \
      \air_date = excluded.air_date, \
      \season = excluded.season, \
      \kind = excluded.kind, \
      \mikan_id = excluded.mikan_id, \
      \tmdb_id = excluded.tmdb_id, \
      \poster_url = excluded.poster_url \
      \RETURNING id, created_at"
      ( bangumi.titleChs,
        bangumi.titleJap,
        bangumi.airDate,
        bangumi.season,
        bangumi.kind,
        bangumi.mikanId,
        bangumi.tmdbId,
        bangumi.bgmtvId,
        bangumi.posterUrl
      )
  case results of
    [(bid, ts)] -> pure (Types.BangumiId bid, ts)
    _ -> error "upsertBangumi: unexpected result"

deleteBangumi ::
  (SqliteTransaction :> es, IOE :> es) =>
  Types.BangumiId ->
  Eff es ()
deleteBangumi (Types.BangumiId bid) =
  execute "DELETE FROM bangumi WHERE id = ?" (Only bid)
