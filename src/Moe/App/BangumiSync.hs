module Moe.App.BangumiSync
  ( syncBangumiSeason,
    fetchAndStoreBangumiSeason,
  )
where

import Control.Monad (forM_, void)
import Data.Text qualified as T
import Data.Text.Conversions (ToText (..))
import Effectful
import Effectful.Concurrent (Concurrent)
import Effectful.Error.Static (Error)
import Effectful.Log (Log)
import Effectful.Log qualified as Log
import Effectful.Sqlite (Sqlite, notransact, transact)
import Moe.Adapter.Database.Bangumi qualified as DB
import Moe.App.Error (MoeError)
import Moe.Domain.Bangumi.Types (Bangumi (..), BangumiKind (..), BangumiSeason, BgmtvId (..), TmdbId (..))
import Moe.Effect.Metadata (Metadata, fetchBangumiDataBySeason, getBgmtvDetail, getTmdbMovieDetail, getTmdbTvDetail)
import Moe.Infra.BangumiData.Types (toBangumi)
import Network.Tmdb.Types.Image qualified as TmdbImage
import Network.Tmdb.Types.Movie qualified as TmdbMovie
import Network.Tmdb.Types.Tv qualified as TmdbTv
import Web.Bgmtv.Types qualified as Bgmtv

syncBangumiSeason ::
  ( Metadata :> es,
    Sqlite :> es,
    Concurrent :> es,
    Log :> es,
    Error MoeError :> es,
    IOE :> es
  ) =>
  Bool ->
  BangumiSeason ->
  Eff es [Bangumi]
syncBangumiSeason forceRefresh season = do
  Log.logInfo_ $ "Syncing bangumi for season: " <> toText season
  existing <- notransact $ DB.listBangumiBySeason season
  if null existing || forceRefresh
    then fetchAndStoreBangumiSeason season
    else do
      Log.logInfo_ $ "Found " <> T.pack (show (length existing)) <> " existing bangumi"
      pure existing

fetchAndStoreBangumiSeason ::
  ( Metadata :> es,
    Sqlite :> es,
    Concurrent :> es,
    Log :> es,
    Error MoeError :> es,
    IOE :> es
  ) =>
  BangumiSeason ->
  Eff es [Bangumi]
fetchAndStoreBangumiSeason season = do
  Log.logInfo_ $ "Fetching bangumi-data for season: " <> toText season
  items <- fetchBangumiDataBySeason season
  Log.logInfo_ $ "Found " <> T.pack (show (length items)) <> " items from bangumi-data"
  let basicBangumis = map toBangumi items
  enrichedBangumis <- mapM enrichWithDetails basicBangumis
  transact $ do
    forM_ enrichedBangumis $ \b -> do
      void $ DB.upsertBangumi b
  Log.logInfo_ $ "Stored " <> T.pack (show (length enrichedBangumis)) <> " bangumi"
  pure enrichedBangumis

enrichWithDetails ::
  (Metadata :> es, IOE :> es) =>
  Bangumi ->
  Eff es Bangumi
enrichWithDetails bangumi = do
  case bangumi.bgmtvId of
    Just (BgmtvId bid) -> do
      mDetail <- getBgmtvDetail bid
      case mDetail of
        Just detail -> pure $ applyBgmtvDetail detail bangumi
        Nothing -> tryTmdbEnrich bangumi
    Nothing -> tryTmdbEnrich bangumi

tryTmdbEnrich ::
  (Metadata :> es, IOE :> es) =>
  Bangumi ->
  Eff es Bangumi
tryTmdbEnrich bangumi =
  case bangumi.tmdbId of
    Just (TmdbId tid) -> do
      case bangumi.kind of
        Tv -> do
          mDetail <- getTmdbTvDetail tid
          pure $ maybe bangumi (`applyTmdbTvDetail` bangumi) mDetail
        Movie -> do
          mDetail <- getTmdbMovieDetail tid
          pure $ maybe bangumi (`applyTmdbMovieDetail` bangumi) mDetail
    Nothing -> pure bangumi

applyBgmtvDetail :: Bgmtv.SubjectDetail -> Bangumi -> Bangumi
applyBgmtvDetail detail b =
  b
    { posterUrl = Just detail.images.large,
      titleChs = if detail.nameCn == "" then b.titleChs else detail.nameCn,
      titleJap = Just detail.name
    }

applyTmdbTvDetail :: TmdbTv.TvDetail -> Bangumi -> Bangumi
applyTmdbTvDetail detail b =
  b
    { posterUrl = TmdbImage.posterUrl TmdbImage.PosterW500 <$> detail.posterPath,
      seasonNumber = Just $ fromIntegral detail.numberOfSeasons
    }

applyTmdbMovieDetail :: TmdbMovie.MovieDetail -> Bangumi -> Bangumi
applyTmdbMovieDetail detail b =
  b
    { posterUrl = TmdbImage.posterUrl TmdbImage.PosterW500 <$> detail.posterPath
    }
