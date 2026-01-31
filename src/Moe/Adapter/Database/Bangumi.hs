module Moe.Adapter.Database.Bangumi
  ( getBangumi,
    listBangumi,
    listBangumiBySeason,
    createBangumi,
    updateBangumi,
    deleteBangumi,
    upsertBangumi,
  )
where

import Data.List (intercalate)
import Data.Maybe (listToMaybe)
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as T
import Effectful
import Effectful.Sqlite (Only (..), SqliteTransaction, execute, query, query_)
import Moe.Adapter.Database.Orphans ()
import Moe.Domain.Bangumi.Types qualified as Types

bangumiColumns :: Text
bangumiColumns = "id, title_chs, title_jap, air_date, season_number, kind, mikan_id, tmdb_id, bangumi_tv_id, poster_url"

getBangumi ::
  (SqliteTransaction :> es, IOE :> es) =>
  Types.BangumiId ->
  Eff es (Maybe Types.Bangumi)
getBangumi (Types.BangumiId bid) = do
  results <- query (fromString $ "SELECT " <> T.unpack bangumiColumns <> " FROM bangumi WHERE id = ?") (Only bid)
  pure $ listToMaybe results

listBangumi ::
  (SqliteTransaction :> es, IOE :> es) =>
  Eff es [Types.Bangumi]
listBangumi =
  query_ (fromString $ "SELECT " <> T.unpack bangumiColumns <> " FROM bangumi")

listBangumiBySeason ::
  (SqliteTransaction :> es, IOE :> es) =>
  Types.BangumiSeason ->
  Eff es [Types.Bangumi]
listBangumiBySeason (Types.BangumiSeason year season) = do
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
  Types.Bangumi ->
  Eff es Types.BangumiId
createBangumi bangumi = do
  results <-
    query
      "INSERT INTO bangumi (title_chs, title_jap, air_date, season_number, kind, mikan_id, tmdb_id, bangumi_tv_id, poster_url) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?) RETURNING id"
      ( bangumi.titleChs,
        bangumi.titleJap,
        bangumi.airDate,
        bangumi.seasonNumber,
        bangumi.kind,
        bangumi.mikanId,
        bangumi.tmdbId,
        bangumi.bgmtvId,
        bangumi.posterUrl
      )
  case results of
    [Only bid] -> pure $ Types.BangumiId bid
    _ -> error "createBangumi: unexpected result"

updateBangumi ::
  (SqliteTransaction :> es, IOE :> es) =>
  Types.Bangumi ->
  Eff es ()
updateBangumi bangumi =
  case bangumi.id of
    Nothing -> pure ()
    Just (Types.BangumiId bid) ->
      execute
        "UPDATE bangumi SET title_chs = ?, title_jap = ?, air_date = ?, season_number = ?, kind = ?, mikan_id = ?, tmdb_id = ?, bangumi_tv_id = ?, poster_url = ? WHERE id = ?"
        ( bangumi.titleChs,
          bangumi.titleJap,
          bangumi.airDate,
          bangumi.seasonNumber,
          bangumi.kind,
          bangumi.mikanId,
          bangumi.tmdbId,
          bangumi.bgmtvId,
          bangumi.posterUrl,
          bid
        )

upsertBangumi ::
  (SqliteTransaction :> es, IOE :> es) =>
  Types.Bangumi ->
  Eff es Types.BangumiId
upsertBangumi bangumi = do
  results <-
    query
      "INSERT INTO bangumi (title_chs, title_jap, air_date, season_number, kind, mikan_id, tmdb_id, bangumi_tv_id, poster_url) \
      \VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?) \
      \ON CONFLICT (bangumi_tv_id) DO UPDATE SET \
      \title_chs = excluded.title_chs, \
      \title_jap = excluded.title_jap, \
      \air_date = excluded.air_date, \
      \season_number = excluded.season_number, \
      \kind = excluded.kind, \
      \mikan_id = excluded.mikan_id, \
      \tmdb_id = excluded.tmdb_id, \
      \poster_url = excluded.poster_url \
      \RETURNING id"
      ( bangumi.titleChs,
        bangumi.titleJap,
        bangumi.airDate,
        bangumi.seasonNumber,
        bangumi.kind,
        bangumi.mikanId,
        bangumi.tmdbId,
        bangumi.bgmtvId,
        bangumi.posterUrl
      )
  case results of
    [Only bid] -> pure $ Types.BangumiId bid
    _ -> error "upsertBangumi: unexpected result"

deleteBangumi ::
  (SqliteTransaction :> es, IOE :> es) =>
  Types.BangumiId ->
  Eff es ()
deleteBangumi (Types.BangumiId bid) =
  execute "DELETE FROM bangumi WHERE id = ?" (Only bid)
