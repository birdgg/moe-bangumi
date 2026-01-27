module Moe.Infra.Effect.Bangumi
  ( runBangumiSQLite,
  )
where

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Sqlite (SQLite, ExecuteResult (..), Only (..), execute, executeReturning, query, query_)
import Moe.Core.Model.Bangumi qualified as Model
import Moe.Effect.Bangumi (Bangumi (..))
import Moe.Infra.Persistence.Schema ()

runBangumiSQLite
  :: (SQLite :> es)
  => Eff (Bangumi : es) a
  -> Eff es a
runBangumiSQLite = interpret $ \_ -> \case
  GetBangumi (Model.BangumiId bid) -> do
    results <- query "SELECT id, name, year, tmdb_id, bangumi_tv_id, poster_url, overview FROM bangumi WHERE id = ?" (Only bid)
    pure $ listToMaybe results

  ListBangumi ->
    query_ "SELECT id, name, year, tmdb_id, bangumi_tv_id, poster_url, overview FROM bangumi"

  CreateBangumi bangumi -> do
    result <- executeReturning
      "INSERT INTO bangumi (name, year, tmdb_id, bangumi_tv_id, poster_url, overview) VALUES (?, ?, ?, ?, ?, ?)"
      ( bangumi.name,
        bangumi.year,
        bangumi.tmdbId,
        bangumi.bangumiTvId,
        bangumi.posterUrl,
        bangumi.overview
      )
    pure $ Model.BangumiId result.lastRowId

  UpdateBangumi bangumi -> do
    case bangumi.id of
      Nothing -> pure ()
      Just (Model.BangumiId bid) ->
        execute
          "UPDATE bangumi SET name = ?, year = ?, tmdb_id = ?, bangumi_tv_id = ?, poster_url = ?, overview = ? WHERE id = ?"
          ( bangumi.name,
            bangumi.year,
            bangumi.tmdbId,
            bangumi.bangumiTvId,
            bangumi.posterUrl,
            bangumi.overview,
            bid
          )

  DeleteBangumi (Model.BangumiId bid) ->
    execute "DELETE FROM bangumi WHERE id = ?" (Only bid)
