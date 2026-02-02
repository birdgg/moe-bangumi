module Moe.App.CalendarSync
  ( runInitialSeasonSync,
    runMonthlySync,
    syncBangumiSeason,
    fetchAndStoreBangumiSeason,
  )
where

import Control.Applicative ((<|>))
import Control.Exception.Safe qualified as Safe
import Control.Monad (forM_, void)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Time (UTCTime (..), getCurrentTime)
import Data.Time.Calendar (Day, fromGregorian, toGregorian)
import Data.Time.Calendar.Month (Month)
import Data.Time.Calendar.Month qualified as Month
import Effectful
import Effectful.Concurrent (Concurrent, runConcurrent)
import Effectful.Error.Static (Error, runErrorWith, throwError)
import Effectful.Log (Log, Logger)
import Effectful.Log qualified as Log
import Effectful.Sqlite (Sqlite, SqliteDb (..), notransact, runSqlite, transact)
import Moe.App.Env (MoeConfig (..), MoeEnv (..), getSettingPath)
import Moe.App.Logging (LogConfig (..), runLog)
import Moe.Domain.Bangumi.Parser (BgmtvParsedTitle (..))
import Moe.Domain.Bangumi.Types (Bangumi (..), BangumiKind (..), BangumiSeason, BgmtvId (..), TmdbId (..), airDateToBangumiSeason, getCurrentBangumiSeason)
import Moe.Infrastructure.BangumiData.Effect (runBangumiDataHttp, toBangumi)
import Moe.Infrastructure.Database.Bangumi qualified as DB
import Moe.Infrastructure.Metadata.Effect (BgmtvDetailResult (..), Metadata, fetchBangumiDataBySeason, getBgmtvDetail, getTmdbMovieDetail, getTmdbTvDetail, runMetadataHttp)
import Moe.Infrastructure.Setting.Effect (runSettingTVar)
import Moe.Error (MoeError (..))
import Network.Tmdb (MovieId (..), TvShowId (..))
import Network.Tmdb.Types.Image qualified as TmdbImage
import Network.Tmdb.Types.Movie qualified as TmdbMovie
import Network.Tmdb.Types.Tv qualified as TmdbTv
import Relude (ToText (..))
import Web.Bgmtv.Types (SubjectId (..))
import Web.Bgmtv.Types qualified as Bgmtv

runInitialSeasonSync :: Logger -> MoeEnv -> IO ()
runInitialSeasonSync logger env = do
  currentSeason <- getCurrentBangumiSeason
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
      season = airDateToBangumiSeason firstDayOfMonth
  runSeasonSync logger env season "calendar-sync"

runSeasonSync :: Logger -> MoeEnv -> BangumiSeason -> T.Text -> IO ()
runSeasonSync logger env season logPrefix =
  runEff $
    withUnliftStrategy (ConcUnlift Ephemeral Unlimited) $
      runConcurrent $
      runSqlite (DbPool env.dbPool) $
        runLog logPrefix logger env.config.logConfig.logLevel $
          runSettingTVar env.settingVar (getSettingPath env) $
            runErrorWith (\_ (err :: MoeError) -> Log.logAttention_ $ logPrefix <> " error: " <> T.pack (show err)) $
              runErrorWith (\_ (err :: T.Text) -> throwError $ ExternalApiError err) $
                runBangumiDataHttp env.httpManager $
                  runMetadataHttp env.httpManager $ do
                    Log.logInfo_ $ "Syncing season: " <> toText season
                    result <- Safe.tryAny $ syncBangumiSeason False season
                    case result of
                      Left err -> Log.logAttention_ $ logPrefix <> " failed: " <> T.pack (show err)
                      Right _ -> Log.logInfo_ $ logPrefix <> " completed for " <> toText season

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

defaultTvSeasonNumber :: Bangumi -> Bangumi
defaultTvSeasonNumber b = case (b.kind, b.seasonNumber) of
  (Tv, Nothing) -> b {seasonNumber = Just 1}
  _ -> b

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
