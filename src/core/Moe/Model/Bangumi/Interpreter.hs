module Moe.Model.Bangumi.Interpreter
  ( runBangumiQuerySQLite,
    runBangumiUpdateSQLite,
  )
where

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Sqlite (ExecuteResult (..), Only (..), SQLite, execute, executeReturning, query, query_)
import Moe.Model.Bangumi.Orphans ()
import Moe.Model.Bangumi.Query (BangumiQuery (..))
import Moe.Model.Bangumi.Types qualified as Types
import Moe.Model.Bangumi.Update (BangumiUpdate (..))

runBangumiQuerySQLite ::
  (SQLite :> es) =>
  Eff (BangumiQuery : es) a ->
  Eff es a
runBangumiQuerySQLite = interpret $ \_ -> \case
  GetBangumi (Types.BangumiId bid) -> do
    results <- query "SELECT id, name, year, season, mikan_id, tmdb_id, bangumi_tv_id, poster_url, overview FROM bangumi WHERE id = ?" (Only bid)
    pure $ listToMaybe results
  ListBangumi ->
    query_ "SELECT id, name, year, season, mikan_id, tmdb_id, bangumi_tv_id, poster_url, overview FROM bangumi"

runBangumiUpdateSQLite ::
  (SQLite :> es) =>
  Eff (BangumiUpdate : es) a ->
  Eff es a
runBangumiUpdateSQLite = interpret $ \_ -> \case
  CreateBangumi bangumi -> do
    result <-
      executeReturning
        "INSERT INTO bangumi (name, year, season, mikan_id, tmdb_id, bangumi_tv_id, poster_url, overview) VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
        ( bangumi.name,
          bangumi.year,
          bangumi.animeSeason,
          bangumi.mikanId,
          bangumi.tmdbId,
          bangumi.bgmtvId,
          bangumi.posterUrl,
          bangumi.overview
        )
    pure $ Types.BangumiId result.lastRowId
  UpdateBangumi bangumi -> do
    case bangumi.id of
      Nothing -> pure ()
      Just (Types.BangumiId bid) ->
        execute
          "UPDATE bangumi SET name = ?, year = ?, season = ?, mikan_id = ?, tmdb_id = ?, bangumi_tv_id = ?, poster_url = ?, overview = ? WHERE id = ?"
          ( bangumi.name,
            bangumi.year,
            bangumi.animeSeason,
            bangumi.mikanId,
            bangumi.tmdbId,
            bangumi.bgmtvId,
            bangumi.posterUrl,
            bangumi.overview,
            bid
          )
  DeleteBangumi (Types.BangumiId bid) ->
    execute "DELETE FROM bangumi WHERE id = ?" (Only bid)
