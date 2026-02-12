module Moe.Infra.Metadata.Effect
  ( Metadata (..),
    searchBgmtv,
    searchTmdb,
    getBgmtvDetail,
    getBangumiEpisodeOffset,
    getTmdbTvDetail,
    getTmdbMovieDetail,
    fetchBangumiDataByMonth,
    runMetadataHttp,
  )
where

import Data.Time.Calendar.Month (Month)
import Effectful
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.TH (makeEffect)
import Moe.Domain.Bangumi (AirDate, Bangumi (..))
import Moe.Error (AppError (..))
import Moe.Infra.Metadata.BangumiData
import Moe.Infra.Metadata.Bgmtv
import Moe.Infra.Metadata.Types (Keyword, classifyProviderError)
import Moe.Infra.Metadata.Tmdb
import Moe.Infra.Setting.Effect (Setting)
import Moe.Prelude
import Network.HTTP.Client (Manager)
import Network.Tmdb (MovieId, TvShowId)
import Network.Tmdb qualified as Tmdb
import Network.Tmdb.Types.Search (MultiSearchResult (..))
import Web.Bgmtv.Client qualified as Bgmtv
import Web.Bgmtv.Types.Episode (Episode (..), EpisodesResponse (..))
import Web.Bgmtv.Types.Id (SubjectId)
import Web.Bgmtv.Types.Search (SearchResponse (..))
import Web.Bgmtv.Types.Subject (Subject (..))

data Metadata :: Effect where
  SearchBgmtv :: Keyword -> Maybe AirDate -> Metadata m [Bangumi]
  SearchTmdb :: Keyword -> Maybe AirDate -> Metadata m [Bangumi]
  GetBgmtvDetail :: SubjectId -> Metadata m (Maybe Bangumi)
  GetBangumiEpisodeOffset :: SubjectId -> Metadata m Double
  GetTmdbTvDetail :: TvShowId -> Metadata m (Maybe Bangumi)
  GetTmdbMovieDetail :: MovieId -> Metadata m (Maybe Bangumi)
  FetchBangumiDataByMonth :: Month -> Metadata m [Bangumi]

makeEffect ''Metadata

runMetadataHttp ::
  (Concurrent :> es, IOE :> es, Error AppError :> es, Setting :> es) =>
  Manager ->
  Eff (Metadata : es) a ->
  Eff es a
runMetadataHttp manager = interpret $ \_ -> \case
  SearchBgmtv keyword maybeYear -> do
    let req = buildBgmtvRequest keyword
    result <- liftIO $ bgmtvClient.searchSubjects req
    resp <- liftEitherWith (MetadataError . classifyProviderError) result
    let filtered = filterByAirDate maybeYear getDate resp.data_
    pure $ mapMaybe bgmtvSubjectToBangumi filtered
    where
      getDate s = s.date
  SearchTmdb keyword maybeYear -> withTmdbClient manager $ \client -> do
    result <- liftIO $ client.searchMulti keyword
    resp <- liftEitherWith (MetadataError . classifyProviderError) result
    let filtered = filterByAirDate maybeYear getDate resp.results
    pure $ mapMaybe tmdbResultToBangumi filtered
    where
      getDate r = r.firstAirDate <|> r.releaseDate
  GetBgmtvDetail subjectId -> do
    result <- liftIO $ bgmtvClient.getSubject subjectId
    pure $ either (const Nothing) bgmtvDetailToBangumi result
  GetBangumiEpisodeOffset subjectId -> do
    result <- liftIO $ bgmtvClient.getEpisodes subjectId (Just (1 :: Int64)) (Nothing :: Maybe Int64)
    pure $ case result of
      Right resp | (firstEp : _) <- (resp :: EpisodesResponse).data_ -> firstEp.sort - fromMaybe 0 firstEp.ep
      _ -> 0
  GetTmdbTvDetail tvShowId ->
    withTmdbClient manager $ \client -> do
      result <- liftIO $ client.getTvDetail tvShowId
      pure $ rightToMaybe result >>= tmdbTvDetailToBangumi
  GetTmdbMovieDetail movieId ->
    withTmdbClient manager $ \client -> do
      result <- liftIO $ client.getMovieDetail movieId
      pure $ rightToMaybe result >>= tmdbMovieDetailToBangumi
  FetchBangumiDataByMonth month -> do
    result <- fetchBangumiDataMonth manager month
    items <- liftEitherWith MetadataError result
    pure $ mapMaybe toBangumi items

  where
    bgmtvClient = mkBgmtvClient manager

-- | Filter results by air date, keeping all if no date is specified.
filterByAirDate :: Maybe AirDate -> (a -> Maybe AirDate) -> [a] -> [a]
filterByAirDate Nothing _ results = results
filterByAirDate (Just d) getDate results = filter ((Just d ==) . getDate) results

