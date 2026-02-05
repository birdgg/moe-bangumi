module Moe.App.CalendarSync
  ( runInitialSeasonSync,
    runMonthlySync,
    syncAirSeason,
    fetchAndStoreAirSeason,
  )
where

import Control.Exception.Safe qualified as Safe
import Data.Time (UTCTime (..), getCurrentTime)
import Data.Time.Calendar (Day, fromGregorian, toGregorian)
import Data.Time.Calendar.Month (Month)
import Data.Time.Calendar.Month qualified as Month
import Effectful (Eff, IOE, type (:>))
import Effectful.Concurrent (Concurrent)
import Effectful.Error.Static (Error)
import Effectful.Log (Log, Logger)
import Effectful.Log qualified as Log
import Effectful.Sqlite (Sqlite, notransact, transact)
import Moe.App.Effect.Runner (runCalendarSyncEffects)
import Moe.App.Env (MoeEnv)
import Moe.Domain.Bangumi.Parser (BgmtvParsedTitle (..))
import Moe.Domain.Bangumi.Types (AirSeason, Bangumi, BangumiF (..), BangumiKind (..), BgmtvId (..), NewBangumi, SeasonNumber (..), TmdbId (..), airDateToAirSeason, getCurrentAirSeason, withId)
import Moe.Error (MoeError (..))
import Moe.Infrastructure.BangumiData.Effect (toNewBangumi)
import Moe.Infrastructure.Database.Bangumi qualified as DB
import Moe.Infrastructure.Metadata.Effect (BgmtvDetailResult (..), Metadata, fetchBangumiDataBySeason, getBgmtvDetail, getTmdbMovieDetail, getTmdbTvDetail)
import Moe.Prelude
import Network.Tmdb (MovieId (..), TvShowId (..))
import Network.Tmdb.Types.Image qualified as TmdbImage
import Network.Tmdb.Types.Movie qualified as TmdbMovie
import Network.Tmdb.Types.Tv qualified as TmdbTv
import Web.Bgmtv.Types (SubjectId (..))
import Web.Bgmtv.Types qualified as Bgmtv

runInitialSeasonSync :: Logger -> MoeEnv -> IO ()
runInitialSeasonSync logger env = do
  currentSeason <- getCurrentAirSeason
  runSeasonSync logger env currentSeason "initial-sync"

runMonthlySync :: Logger -> MoeEnv -> IO ()
runMonthlySync logger env = do
  now <- getCurrentTime
  let currentMonth = dayToMonth (utctDay now)
  runMonthSyncInternal logger env currentMonth

dayToMonth :: Day -> Month
dayToMonth day =
  let (y, m, _) = toGregorian day
   in Month.YearMonth y m

runMonthSyncInternal :: Logger -> MoeEnv -> Month -> IO ()
runMonthSyncInternal logger env month = do
  let Month.YearMonth year monthNum = month
      firstDayOfMonth = fromGregorian year monthNum 1
      season = airDateToAirSeason firstDayOfMonth
  runSeasonSync logger env season "calendar-sync"

runSeasonSync :: Logger -> MoeEnv -> AirSeason -> Text -> IO ()
runSeasonSync logger env season logPrefix =
  runCalendarSyncEffects env logger logPrefix $ do
    Log.logInfo_ $ "Syncing season: " <> toText season
    result <- Safe.tryAny $ syncAirSeason False season
    case result of
      Left err -> Log.logAttention_ $ logPrefix <> " failed: " <> show err
      Right _ -> Log.logInfo_ $ logPrefix <> " completed for " <> toText season

syncAirSeason ::
  ( Metadata :> es,
    Sqlite :> es,
    Concurrent :> es,
    Log :> es,
    Error MoeError :> es,
    IOE :> es
  ) =>
  Bool ->
  AirSeason ->
  Eff es [Bangumi]
syncAirSeason forceRefresh season = do
  existing <- notransact $ DB.listBangumiBySeason season
  if null existing || forceRefresh
    then fetchAndStoreAirSeason season
    else do
      Log.logInfo_ $ "Found " <> show (length existing) <> " existing bangumi"
      pure existing

fetchAndStoreAirSeason ::
  ( Metadata :> es,
    Sqlite :> es,
    Concurrent :> es,
    Log :> es,
    Error MoeError :> es,
    IOE :> es
  ) =>
  AirSeason ->
  Eff es [Bangumi]
fetchAndStoreAirSeason season = do
  Log.logInfo_ $ "Fetching bangumi-data for season: " <> toText season
  items <- fetchBangumiDataBySeason season
  let basicBangumis = map toNewBangumi items
  enrichedBangumis <- mapM enrichWithDetails basicBangumis
  transact $ do
    forM enrichedBangumis $ \nb -> do
      (bid, ts) <- DB.upsertBangumi nb
      pure $ withId bid ts nb

enrichWithDetails ::
  (Metadata :> es, IOE :> es) =>
  NewBangumi ->
  Eff es NewBangumi
enrichWithDetails bangumi = do
  result <- runMaybeT $ tryBgmtvEnrich <|> tryTmdbEnrich
  pure $ defaultTvSeasonNumber $ fromMaybe bangumi result
  where
    tryBgmtvEnrich = do
      BgmtvId bid <- MaybeT $ pure bangumi.bgmtvId
      detailResult <- MaybeT $ getBgmtvDetail (SubjectId (fromIntegral bid))
      pure $ applyBgmtvDetail detailResult bangumi

    tryTmdbEnrich = do
      TmdbId tid <- MaybeT $ pure bangumi.tmdbId
      case bangumi.kind of
        Movie -> do
          detail <- MaybeT $ getTmdbMovieDetail (MovieId (fromIntegral tid))
          pure $ applyTmdbMovieDetail detail bangumi
        _ -> do
          detail <- MaybeT $ getTmdbTvDetail (TvShowId (fromIntegral tid))
          pure $ applyTmdbTvDetail detail bangumi

defaultTvSeasonNumber :: NewBangumi -> NewBangumi
defaultTvSeasonNumber b = case (b.kind, b.season) of
  (Tv, Nothing) -> b {season = Just (SeasonNumber 1)}
  _ -> b

applyBgmtvDetail :: BgmtvDetailResult -> NewBangumi -> NewBangumi
applyBgmtvDetail BgmtvDetailResult {detail, parsed} b =
  let BgmtvParsedTitle parsedChs parsedJap parsedSeason = parsed
      subjectDetail :: Bgmtv.SubjectDetail
      subjectDetail = detail
   in b
        { posterUrl = Just subjectDetail.images.large,
          titleChs = if parsedChs == "" then b.titleChs else parsedChs,
          titleJap = Just parsedJap,
          season = parsedSeason <|> b.season
        }

applyTmdbTvDetail :: TmdbTv.TvDetail -> NewBangumi -> NewBangumi
applyTmdbTvDetail detail b =
  b
    { posterUrl = TmdbImage.posterUrl TmdbImage.PosterW500 <$> detail.posterPath,
      season = Just $ SeasonNumber $ fromIntegral detail.numberOfSeasons
    }

applyTmdbMovieDetail :: TmdbMovie.MovieDetail -> NewBangumi -> NewBangumi
applyTmdbMovieDetail detail b =
  b
    { posterUrl = TmdbImage.posterUrl TmdbImage.PosterW500 <$> detail.posterPath
    }
