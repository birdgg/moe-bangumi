module Moe.Adapter.Http.Metadata
  ( runMetadataHttp,
  )
where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word16)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static (Error, throwError)
import Moe.App.Error (MoeError (..))
import Moe.Domain.Bangumi.Types qualified as BangumiTypes
import Moe.Domain.Setting.Types (TMDBConfig (..))
import Moe.Domain.Setting.Types qualified as Setting
import Moe.Effect.Metadata (Metadata (..))
import Moe.Effect.Setting (Setting, getSetting)
import Moe.Infra.BangumiData.Client qualified as BangumiDataClient
import Moe.Infra.BangumiData.Types (BangumiDataItem (..), TitleTranslate (..))
import Network.HTTP.Client (Manager)
import Network.Tmdb qualified as Tmdb
import Web.Bgmtv.Client qualified as Bgmtv
import Web.Bgmtv.Types qualified as Bgmtv

bgmtvUserAgent :: Text
bgmtvUserAgent = "moe-bangumi/0.1.0"

mkBgmtvClient :: Manager -> Bgmtv.BgmtvClient
mkBgmtvClient = Bgmtv.newBgmtvClientWith ?? Bgmtv.defaultConfig bgmtvUserAgent
  where
    (??) = flip

runMetadataHttp ::
  (IOE :> es, Error MoeError :> es, Setting :> es) =>
  Manager ->
  Eff (Metadata : es) a ->
  Eff es a
runMetadataHttp manager = interpret $ \_ -> \case
  SearchBgmtv keyword maybeYear -> do
    let bgmtvClient = mkBgmtvClient manager
        req = buildBgmtvRequest keyword maybeYear
    result <- liftIO $ bgmtvClient.searchSubjects req
    case result of
      Left err -> throwError $ ExternalApiError ("Bgmtv search failed: " <> T.pack (show err))
      Right resp -> pure resp.data_
  SearchTmdb keyword _maybeYear -> do
    pref <- getSetting
    case Setting.tmdb pref of
      Nothing -> throwError $ ExternalApiError "TMDB API key not configured"
      Just cfg -> do
        let client = mkTmdbApi cfg manager
        result <- liftIO $ client.searchMulti keyword
        case result of
          Left err -> throwError $ ExternalApiError ("TMDB search failed: " <> T.pack (show err))
          Right resp -> pure resp.results
  SearchBangumiData keyword maybeYear -> do
    let targetYear = fromMaybe 2024 maybeYear
        months = [1 .. 12]
    result <- liftIO $ BangumiDataClient.fetchByMonths targetYear months
    case result of
      Left err -> throwError $ ExternalApiError ("BangumiData fetch failed: " <> T.pack err)
      Right items -> pure $ filter (matchesKeyword keyword) items
  GetBgmtvDetail bgmtvId -> do
    let bgmtvClient = mkBgmtvClient manager
    result <- liftIO $ bgmtvClient.getSubject (Bgmtv.SubjectId (fromIntegral bgmtvId))
    pure $ either (const Nothing) Just result
  GetTmdbTvDetail tmdbId -> do
    pref <- getSetting
    case Setting.tmdb pref of
      Nothing -> pure Nothing
      Just cfg -> do
        let client = mkTmdbApi cfg manager
        result <- liftIO $ client.getTvDetail (Tmdb.TvShowId (fromIntegral tmdbId))
        pure $ handleTmdbResult result
  GetTmdbMovieDetail tmdbId -> do
    pref <- getSetting
    case Setting.tmdb pref of
      Nothing -> pure Nothing
      Just cfg -> do
        let client = mkTmdbApi cfg manager
        result <- liftIO $ client.getMovieDetail (Tmdb.MovieId (fromIntegral tmdbId))
        pure $ handleTmdbResult result
  FetchBangumiDataBySeason season -> do
    let year = fromIntegral season.year
        months = BangumiTypes.seasonToMonths season.season
    result <- liftIO $ BangumiDataClient.fetchByMonths year months
    case result of
      Left err -> throwError $ ExternalApiError ("BangumiData fetch failed: " <> T.pack err)
      Right items -> pure items

handleTmdbResult :: Either Tmdb.TmdbError a -> Maybe a
handleTmdbResult = either (const Nothing) Just

mkTmdbApi :: Setting.TMDBConfig -> Manager -> Tmdb.TmdbApi
mkTmdbApi cfg = Tmdb.newTmdbApi (Tmdb.TmdbConfig cfg.apiKey Tmdb.zhCN)

buildBgmtvRequest :: Text -> Maybe Word16 -> Bgmtv.SearchRequest
buildBgmtvRequest keyword maybeYear =
  Bgmtv.SearchRequest
    { keyword = keyword,
      filter_ =
        Just
          Bgmtv.SearchFilter
            { subjectType = Just [Bgmtv.Anime],
              metaTags = Nothing,
              airDate = fmap buildYearFilter maybeYear
            }
    }

buildYearFilter :: Word16 -> [Text]
buildYearFilter year =
  [ ">=" <> T.pack (show year) <> "-01-01",
    "<=" <> T.pack (show year) <> "-12-31"
  ]

matchesKeyword :: Text -> BangumiDataItem -> Bool
matchesKeyword keyword item =
  let kw = T.toLower keyword
      titleMatch = kw `T.isInfixOf` T.toLower item.title
      TitleTranslate hans hant = item.titleTranslate
      zhHansMatch = any (T.isInfixOf kw . T.toLower) hans
      zhHantMatch = any (T.isInfixOf kw . T.toLower) hant
   in titleMatch || zhHansMatch || zhHantMatch
