module Moe.Effect.Metadata
  ( Metadata (..),
    searchBgmtv,
    searchTmdb,
    searchBangumiData,
    getBgmtvDetail,
    getTmdbTvDetail,
    getTmdbMovieDetail,
    fetchBangumiDataBySeason,
    runMetadataHttp,
  )
where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word16, Word32)
import Effectful
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Error.Static (Error, throwError)
import Effectful.TH (makeEffect)
import Moe.App.Error (MoeError (..))
import Moe.Domain.Bangumi.Types (BangumiSeason)
import Moe.Domain.Bangumi.Types qualified as BangumiTypes
import Moe.Domain.Setting.Types qualified as Setting
import Moe.Effect.BangumiData qualified as BangumiData
import Moe.Effect.Setting (Setting, getSetting)
import Moe.Infra.BangumiData.Types (BangumiDataItem (..), TitleTranslate (..))
import Network.HTTP.Client (Manager)
import Network.Tmdb qualified as Tmdb
import Network.Tmdb.Types.Movie (MovieDetail)
import Network.Tmdb.Types.Search (MultiSearchResult)
import Network.Tmdb.Types.Tv (TvDetail)
import Web.Bgmtv.Client qualified as Bgmtv
import Web.Bgmtv.Types (Subject, SubjectDetail)
import Web.Bgmtv.Types qualified as Bgmtv

data Metadata :: Effect where
  SearchBgmtv :: Text -> Maybe Word16 -> Metadata m [Subject]
  SearchTmdb :: Text -> Maybe Word16 -> Metadata m [MultiSearchResult]
  SearchBangumiData :: Text -> Maybe Word16 -> Metadata m [BangumiDataItem]
  GetBgmtvDetail :: Word32 -> Metadata m (Maybe SubjectDetail)
  GetTmdbTvDetail :: Word32 -> Metadata m (Maybe TvDetail)
  GetTmdbMovieDetail :: Word32 -> Metadata m (Maybe MovieDetail)
  FetchBangumiDataBySeason :: BangumiSeason -> Metadata m [BangumiDataItem]

makeEffect ''Metadata

bgmtvUserAgent :: Text
bgmtvUserAgent = "moe-bangumi/0.1.0"

mkBgmtvClient :: Manager -> Bgmtv.BgmtvClient
mkBgmtvClient = Bgmtv.newBgmtvClientWith ?? Bgmtv.defaultConfig bgmtvUserAgent
  where
    (??) = flip

runMetadataHttp ::
  (IOE :> es, Error MoeError :> es, Setting :> es, BangumiData.BangumiData :> es) =>
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
    result <- BangumiData.fetchByMonths targetYear months
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
    result <- BangumiData.fetchByMonths year months
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
