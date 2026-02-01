module Moe.Effect.Metadata
  ( Metadata (..),
    BgmtvSearchResult (..),
    BgmtvDetailResult (..),
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
import Data.Time.Calendar (Year)
import Data.Time.Calendar.Month qualified as Month
import Effectful
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Error.Static (Error, throwError)
import Effectful.TH (makeEffect)
import GHC.Generics (Generic)
import Moe.Error (MoeError (..), liftError)
import Moe.Domain.Bangumi.Parser (BgmtvParsedTitle, parseBgmtvTitle)
import Moe.Domain.Bangumi.Types (BangumiSeason)
import Moe.Domain.Bangumi.Types qualified as BangumiTypes
import Moe.Domain.Setting.Types qualified as Setting
import Moe.Effect.BangumiData (BangumiDataItem (..), TitleTranslate (..))
import Moe.Effect.BangumiData qualified as BangumiData
import Moe.Effect.Setting (Setting, getSetting)
import Network.HTTP.Client (Manager)
import Network.Tmdb (MovieId, TvShowId)
import Network.Tmdb qualified as Tmdb
import Network.Tmdb.Types.Movie (MovieDetail)
import Network.Tmdb.Types.Search (MultiSearchResult)
import Network.Tmdb.Types.Tv (TvDetail)
import Web.Bgmtv.Client qualified as Bgmtv
import Web.Bgmtv.Types (Subject, SubjectDetail, SubjectId)
import Web.Bgmtv.Types qualified as Bgmtv

data BgmtvSearchResult = BgmtvSearchResult
  { subject :: Subject,
    parsed :: BgmtvParsedTitle
  }
  deriving stock (Eq, Show, Generic)

data BgmtvDetailResult = BgmtvDetailResult
  { detail :: SubjectDetail,
    parsed :: BgmtvParsedTitle
  }
  deriving stock (Eq, Show, Generic)

data Metadata :: Effect where
  SearchBgmtv :: Text -> Maybe Year -> Metadata m [BgmtvSearchResult]
  SearchTmdb :: Text -> Maybe Year -> Metadata m [MultiSearchResult]
  SearchBangumiData :: Text -> Maybe Year -> Metadata m [BangumiDataItem]
  GetBgmtvDetail :: SubjectId -> Metadata m (Maybe BgmtvDetailResult)
  GetTmdbTvDetail :: TvShowId -> Metadata m (Maybe TvDetail)
  GetTmdbMovieDetail :: MovieId -> Metadata m (Maybe MovieDetail)
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
    resp <- liftError ExternalApiError "Bgmtv search failed: " result
    pure $ map mkSearchResult resp.data_
    where
      mkSearchResult s = BgmtvSearchResult s (parseBgmtvTitle (s.name, s.nameCn))
  SearchTmdb keyword _maybeYear -> do
    pref <- getSetting
    case Setting.tmdb pref of
      Nothing -> throwError $ ExternalApiError "TMDB API key not configured"
      Just cfg -> do
        let client = mkTmdbApi cfg manager
        result <- liftIO $ client.searchMulti keyword
        resp <- liftError ExternalApiError "TMDB search failed: " result
        pure resp.results
  SearchBangumiData keyword maybeYear -> do
    let targetYear = fromMaybe 2024 maybeYear
        months = [Month.YearMonth targetYear m | m <- [1 .. 12]]
    items <- BangumiData.fetchByMonths months
    pure $ filter (matchesKeyword keyword) items
  GetBgmtvDetail subjectId -> do
    let bgmtvClient = mkBgmtvClient manager
    result <- liftIO $ bgmtvClient.getSubject subjectId
    pure $ either (const Nothing) (Just . mkDetailResult) result
    where
      mkDetailResult d = BgmtvDetailResult d (parseBgmtvTitle (d.name, d.nameCn))
  GetTmdbTvDetail tvShowId -> do
    pref <- getSetting
    case Setting.tmdb pref of
      Nothing -> pure Nothing
      Just cfg -> do
        let client = mkTmdbApi cfg manager
        result <- liftIO $ client.getTvDetail tvShowId
        pure $ handleTmdbResult result
  GetTmdbMovieDetail movieId -> do
    pref <- getSetting
    case Setting.tmdb pref of
      Nothing -> pure Nothing
      Just cfg -> do
        let client = mkTmdbApi cfg manager
        result <- liftIO $ client.getMovieDetail movieId
        pure $ handleTmdbResult result
  FetchBangumiDataBySeason season -> do
    let months = [Month.YearMonth season.year m | m <- BangumiTypes.seasonToMonths season.season]
    BangumiData.fetchByMonths months

handleTmdbResult :: Either Tmdb.TmdbError a -> Maybe a
handleTmdbResult = either (const Nothing) Just

mkTmdbApi :: Setting.TMDBConfig -> Manager -> Tmdb.TmdbApi
mkTmdbApi cfg = Tmdb.newTmdbApi (Tmdb.TmdbConfig cfg.apiKey Tmdb.zhCN)

buildBgmtvRequest :: Text -> Maybe Year -> Bgmtv.SearchRequest
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

buildYearFilter :: Year -> [Text]
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
