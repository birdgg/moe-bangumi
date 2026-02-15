-- | Calendar service: fetches, enriches, and stores bangumi data by season.
module Moe.App.Calendar
  ( syncAirSeason,
  )
where

import Data.Time.Calendar.Month qualified as Month
import Effectful (type (:>))
import Effectful.Concurrent.Async (forConcurrently)
import Effectful.Concurrent.QSem (newQSem, signalQSem, waitQSem)
import Effectful.Exception (bracket_)
import Effectful.Log qualified as Log
import Effectful.Sqlite (transact)
import Moe.Domain.Bangumi (AirSeason (..), Bangumi (..), BangumiKind (..), BgmtvId (..), SeasonNumber (..), TmdbId (..), seasonToMonths)
import Moe.Domain.Shared.Entity (Entity (..))
import Moe.Infra.Database.Bangumi qualified as DB
import Moe.Infra.Metadata.Effect (Metadata, fetchBangumiDataByMonth, getBgmtvDetail, getTmdbMovieDetail, getTmdbTvDetail)
import Moe.Prelude
import Network.Tmdb (MovieId (..), TvShowId (..))
import Web.Bgmtv.Types.Id (SubjectId (..))

-- | Fetch bangumi data for a season, enrich with metadata, and store in database.
syncAirSeason ::
  ( Metadata :> es,
    Sqlite :> es,
    Concurrent :> es,
    Log :> es,
    IOE :> es
  ) =>
  AirSeason ->
  Eff es [Entity Bangumi]
syncAirSeason season = do
  Log.logInfo_ $ "Fetching bangumi-data for season: " <> toText season
  let months = [Month.YearMonth season.year m | m <- seasonToMonths season.season]
  basicBangumis <- concat <$> mapM fetchBangumiDataByMonth months
  sem <- newQSem 5
  enrichedBangumis <- forConcurrently basicBangumis $ \b ->
    bracket_ (waitQSem sem) (signalQSem sem) $
      enrichWithDetails b
  transact $ do
    forM enrichedBangumis $ \nb -> do
      (bid, ts, uts) <- DB.upsertBangumi nb
      pure $ Entity bid nb ts uts

enrichWithDetails ::
  (Metadata :> es, IOE :> es) =>
  Bangumi ->
  Eff es Bangumi
enrichWithDetails bangumi = do
  result <- runMaybeT $ tryBgmtvEnrich <|> tryTmdbEnrich
  pure $ defaultTvSeasonNumber $ fromMaybe bangumi result
  where
    -- Word32 -> Int64 widening is always safe, no truncation possible.
    tryBgmtvEnrich = do
      BgmtvId bid <- MaybeT $ pure bangumi.bgmtvId
      detailResult <- MaybeT $ getBgmtvDetail (SubjectId (fromIntegral bid))
      pure $ applyBgmtvDetail detailResult bangumi

    tryTmdbEnrich = do
      TmdbId tid <- MaybeT $ pure bangumi.tmdbId
      case bangumi.kind of
        Movie -> do
          detailResult <- MaybeT $ getTmdbMovieDetail (MovieId (fromIntegral tid))
          pure $ applyTmdbDetail detailResult bangumi
        _ -> do
          detailResult <- MaybeT $ getTmdbTvDetail (TvShowId (fromIntegral tid))
          pure $ applyTmdbDetail detailResult bangumi

defaultTvSeasonNumber :: Bangumi -> Bangumi
defaultTvSeasonNumber b = case (b.kind, b.season) of
  (Tv, Nothing) -> b {season = Just (SeasonNumber 1)}
  _ -> b

-- | Enrich a Bangumi with detail fields from Bgmtv.
applyBgmtvDetail :: Bangumi -> Bangumi -> Bangumi
applyBgmtvDetail detail b =
  b
    { posterUrl = detail.posterUrl <|> b.posterUrl,
      titleChs = fromMaybe b.titleChs $ guarded (/= "") detail.titleChs,
      titleJap = detail.titleJap <|> b.titleJap,
      season = detail.season <|> b.season
    }

-- | Enrich a Bangumi with detail fields from TMDB.
applyTmdbDetail :: Bangumi -> Bangumi -> Bangumi
applyTmdbDetail detail b =
  b
    { posterUrl = detail.posterUrl <|> b.posterUrl,
      season = detail.season <|> b.season,
      firstAirYear = detail.firstAirYear <|> b.firstAirYear
    }
