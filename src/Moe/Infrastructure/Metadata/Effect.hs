module Moe.Infrastructure.Metadata.Effect
  ( Metadata (..),
    BgmtvSearchResult (..),
    BgmtvDetailResult (..),
    searchBgmtv,
    searchTmdb,
    searchBangumiData,
    getBgmtvDetail,
    getBangumiEpisodeOffset,
    getTmdbTvDetail,
    getTmdbMovieDetail,
    fetchBangumiDataBySeason,
    runMetadataHttp,
  )
where

import Data.Text qualified as T
import Data.Time.Calendar (Year)
import Data.Time.Calendar.Month qualified as Month
import Effectful
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Error.Static (Error, throwError)
import Effectful.TH (makeEffect)
import Moe.Domain.Bangumi.Parser (BgmtvParsedTitle, parseBgmtvTitle)
import Moe.Domain.Bangumi.Types (AirSeason)
import Moe.Domain.Bangumi.Types qualified as BangumiTypes
import Moe.Domain.Setting.Types qualified as Setting
import Moe.Error (MoeError (..), liftError)
import Moe.Infrastructure.BangumiData.Effect (BangumiDataItem (..), TitleTranslate (..))
import Moe.Infrastructure.BangumiData.Effect qualified as BangumiData
import Moe.Infrastructure.Setting.Effect (Setting, getSetting)
import Moe.Prelude
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
  GetBangumiEpisodeOffset :: SubjectId -> Metadata m Double
  GetTmdbTvDetail :: TvShowId -> Metadata m (Maybe TvDetail)
  GetTmdbMovieDetail :: MovieId -> Metadata m (Maybe MovieDetail)
  FetchBangumiDataBySeason :: AirSeason -> Metadata m [BangumiDataItem]

makeEffect ''Metadata

bgmtvUserAgent :: Text
bgmtvUserAgent = "moe-bangumi/0.1.0"

mkBgmtvClient :: Manager -> Bgmtv.BgmtvClient
mkBgmtvClient = Bgmtv.newBgmtvClientWith ?? Bgmtv.defaultConfig bgmtvUserAgent

runMetadataHttp ::
  (IOE :> es, Error MoeError :> es, Setting :> es, BangumiData.BangumiData :> es) =>
  Manager ->
  Eff (Metadata : es) a ->
  Eff es a
runMetadataHttp manager = interpret $ \_ -> \case
  SearchBgmtv keyword maybeYear -> do
    let bgmtvClient = mkBgmtvClient manager
        req = buildBgmtvRequest keyword
    result <- liftIO $ bgmtvClient.searchSubjects req
    resp <- liftError ExternalApiError "Bgmtv search failed: " result
    pure $ filterByYear maybeYear getYear $ map mkSearchResult resp.data_
    where
      mkSearchResult s = BgmtvSearchResult s (parseBgmtvTitle (s.name, s.nameCn))
      getYear r = r.subject.date >>= extractYear
  SearchTmdb keyword maybeYear -> do
    pref <- getSetting
    case Setting.tmdb pref of
      Nothing -> throwError $ ExternalApiError "TMDB API key not configured"
      Just cfg -> do
        let client = mkTmdbApi cfg manager
        result <- liftIO $ client.searchMulti keyword
        resp <- liftError ExternalApiError "TMDB search failed: " result
        pure $ filterByYear maybeYear getYear resp.results
    where
      getYear r = extractYear =<< (r.firstAirDate <|> r.releaseDate)
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
  GetBangumiEpisodeOffset subjectId -> do
    let bgmtvClient = mkBgmtvClient manager
    result <- liftIO $ bgmtvClient.getEpisodes subjectId (Just 1) Nothing
    pure $ case result of
      Right resp | (ep : _) <- resp.data_ -> ep.sort - fromMaybe 0 ep.ep
      _ -> 0
  GetTmdbTvDetail tvShowId ->
    withTmdbClient manager $ \client -> client.getTvDetail tvShowId
  GetTmdbMovieDetail movieId ->
    withTmdbClient manager $ \client -> client.getMovieDetail movieId
  FetchBangumiDataBySeason season -> do
    let months = [Month.YearMonth season.year m | m <- BangumiTypes.seasonToMonths season.season]
    BangumiData.fetchByMonths months

handleTmdbResult :: Either Tmdb.TmdbError a -> Maybe a
handleTmdbResult = either (const Nothing) Just

withTmdbClient ::
  (Setting :> es, IOE :> es) =>
  Manager ->
  (Tmdb.TmdbApi -> IO (Either Tmdb.TmdbError a)) ->
  Eff es (Maybe a)
withTmdbClient manager action = do
  pref <- getSetting
  case Setting.tmdb pref of
    Nothing -> pure Nothing
    Just cfg -> do
      let client = mkTmdbApi cfg manager
      result <- liftIO $ action client
      pure $ handleTmdbResult result

mkTmdbApi :: Setting.TMDBConfig -> Manager -> Tmdb.TmdbApi
mkTmdbApi cfg = Tmdb.newTmdbApi (Tmdb.TmdbConfig cfg.apiKey Tmdb.zhCN)

buildBgmtvRequest :: Text -> Bgmtv.SearchRequest
buildBgmtvRequest keyword =
  Bgmtv.SearchRequest
    { keyword = keyword,
      filter_ =
        Just
          Bgmtv.SearchFilter
            { subjectType = Just [Bgmtv.Anime],
              metaTags = Nothing,
              airDate = Nothing
            }
    }

matchesKeyword :: Text -> BangumiDataItem -> Bool
matchesKeyword keyword item =
  let kw = T.toLower keyword
      titleMatch = kw `T.isInfixOf` T.toLower item.title
      TitleTranslate hans hant = item.titleTranslate
      zhHansMatch = any (T.isInfixOf kw . T.toLower) hans
      zhHantMatch = any (T.isInfixOf kw . T.toLower) hant
   in titleMatch || zhHansMatch || zhHantMatch

filterByYear :: Maybe Year -> (a -> Maybe Year) -> [a] -> [a]
filterByYear Nothing _ results = results
filterByYear (Just y) getYear results = filter ((Just y ==) . getYear) results

extractYear :: Text -> Maybe Year
extractYear = readMaybe . T.unpack . T.take 4
