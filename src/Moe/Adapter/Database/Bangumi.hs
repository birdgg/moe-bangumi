module Moe.Adapter.Database.Bangumi
  ( getBangumi,
    listBangumi,
    createBangumi,
    updateBangumi,
    deleteBangumi,
  )
where

import Effectful
import Effectful.Sqlite (Only (..), SqliteTransaction, execute, query, query_)
import Moe.Adapter.Database.Orphans ()
import Moe.Domain.Bangumi.Types qualified as Types

getBangumi ::
  (SqliteTransaction :> es, IOE :> es) =>
  Types.BangumiId ->
  Eff es (Maybe Types.Bangumi)
getBangumi (Types.BangumiId bid) = do
  results <- query "SELECT id, name, air_date, mikan_id, tmdb_id, bangumi_tv_id, poster_url FROM bangumi WHERE id = ?" (Only bid)
  pure $ listToMaybe results

listBangumi ::
  (SqliteTransaction :> es, IOE :> es) =>
  Eff es [Types.Bangumi]
listBangumi =
  query_ "SELECT id, name, air_date, mikan_id, tmdb_id, bangumi_tv_id, poster_url FROM bangumi"

createBangumi ::
  (SqliteTransaction :> es, IOE :> es) =>
  Types.Bangumi ->
  Eff es Types.BangumiId
createBangumi bangumi = do
  results <-
    query
      "INSERT INTO bangumi (name, air_date, mikan_id, tmdb_id, bangumi_tv_id, poster_url) VALUES (?, ?, ?, ?, ?, ?) RETURNING id"
      ( bangumi.name,
        bangumi.airDate,
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
    Nothing -> pass
    Just (Types.BangumiId bid) ->
      execute
        "UPDATE bangumi SET name = ?, air_date = ?, mikan_id = ?, tmdb_id = ?, bangumi_tv_id = ?, poster_url = ? WHERE id = ?"
        ( bangumi.name,
          bangumi.airDate,
          bangumi.mikanId,
          bangumi.tmdbId,
          bangumi.bgmtvId,
          bangumi.posterUrl,
          bid
        )

deleteBangumi ::
  (SqliteTransaction :> es, IOE :> es) =>
  Types.BangumiId ->
  Eff es ()
deleteBangumi (Types.BangumiId bid) =
  execute "DELETE FROM bangumi WHERE id = ?" (Only bid)
