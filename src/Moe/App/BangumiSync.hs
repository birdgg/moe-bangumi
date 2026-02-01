module Moe.App.BangumiSync
  ( syncBangumiSeason,
    fetchAndStoreBangumiSeason,
  )
where

import Control.Applicative ((<|>))
import Control.Monad (forM_, void)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Text.Conversions (ToText (..))
import Effectful
import Effectful.Concurrent (Concurrent)
import Effectful.Error.Static (Error)
import Effectful.Log (Log)
import Effectful.Log qualified as Log
import Effectful.Sqlite (Sqlite, notransact, transact)
import Moe.Adapter.Database.Bangumi qualified as DB
import Moe.Error (MoeError)
import Moe.Domain.Bangumi.Parser (BgmtvParsedTitle (..))
import Moe.Domain.Bangumi.Types (Bangumi (..), BangumiKind (..), BangumiSeason, BgmtvId (..), TmdbId (..))
import Moe.Effect.Metadata (BgmtvDetailResult (..), Metadata, fetchBangumiDataBySeason, getBgmtvDetail, getTmdbMovieDetail, getTmdbTvDetail)
import Moe.Effect.BangumiData (toBangumi)
import Network.Tmdb (MovieId (..), TvShowId (..))
import Network.Tmdb.Types.Image qualified as TmdbImage
import Network.Tmdb.Types.Movie qualified as TmdbMovie
import Network.Tmdb.Types.Tv qualified as TmdbTv
import Web.Bgmtv.Types (SubjectId (..))
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
  let basicBangumis = map toBangumi items
  enrichedBangumis <- mapM enrichWithDetails basicBangumis
  transact $ do
    forM_ enrichedBangumis $ \b -> do
      void $ DB.upsertBangumi b
  pure enrichedBangumis

enrichWithDetails ::
  (Metadata :> es, IOE :> es) =>
  Bangumi ->
  Eff es Bangumi
enrichWithDetails bangumi = do
  result <- runMaybeT $ tryBgmtvEnrich <|> tryTmdbEnrich
  pure $ fromMaybe bangumi result
  where
    tryBgmtvEnrich = do
      BgmtvId bid <- MaybeT $ pure bangumi.bgmtvId
      result <- MaybeT $ getBgmtvDetail (SubjectId (fromIntegral bid))
      pure $ applyBgmtvDetail result bangumi

    tryTmdbEnrich = do
      TmdbId tid <- MaybeT $ pure bangumi.tmdbId
      case bangumi.kind of
        Tv -> do
          detail <- MaybeT $ getTmdbTvDetail (TvShowId (fromIntegral tid))
          pure $ applyTmdbTvDetail detail bangumi
        Movie -> do
          detail <- MaybeT $ getTmdbMovieDetail (MovieId (fromIntegral tid))
          pure $ applyTmdbMovieDetail detail bangumi

applyBgmtvDetail :: BgmtvDetailResult -> Bangumi -> Bangumi
applyBgmtvDetail BgmtvDetailResult {detail, parsed} b =
  let BgmtvParsedTitle parsedChs parsedJap parsedSeason = parsed
      subjectDetail :: Bgmtv.SubjectDetail
      subjectDetail = detail
   in b
        { posterUrl = Just subjectDetail.images.large,
          titleChs = if parsedChs == "" then b.titleChs else parsedChs,
          titleJap = Just parsedJap,
          seasonNumber = parsedSeason <|> b.seasonNumber
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
