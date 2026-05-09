module Moe.Libs.Tmdb.Types
  ( TmdbClientError (..),
    TmdbApiError (..),
    fromClientError,
    MovieId (..),
    TvShowId (..),
    PaginatedResponse (..),
    TmdbLocale (..),
    PosterSize (..),
    posterUrl,
    MovieDetail (..),
    MultiSearchResult (..),
    MediaType (..),
    TvDetail (..),
  )
where

import Data.Aeson (FromJSON (..), Key, Value (..), eitherDecode, parseJSON, withObject, withText, (.:), (.:?))
import Data.Aeson.Types (Object, Parser, parseMaybe)
import Data.Text qualified as T
import Data.Time.Calendar (Day)
import Moe.Prelude
import Network.HTTP.Types.Status qualified as HTTP
import Servant.API (FromHttpApiData)
import Servant.Client (ClientError (..))
import Servant.Client.Core (ResponseF (..))
import Web.HttpApiData (ToHttpApiData)

data TmdbClientError
  = ServantError ClientError
  | TmdbError TmdbApiError
  deriving stock (Show, Generic)

data TmdbApiError = TmdbApiError
  { httpStatus :: Int,
    statusCode :: Int64,
    statusMessage :: Text
  }
  deriving stock (Show, Eq, Generic)

data ApiErrorResponse = ApiErrorResponse
  { statusCode :: Int64,
    statusMessage :: Text
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON ApiErrorResponse where
  parseJSON = withObject "ApiErrorResponse" $ \o ->
    ApiErrorResponse
      <$> o .: "status_code"
      <*> o .: "status_message"

fromClientError :: ClientError -> TmdbClientError
fromClientError err@(FailureResponse _req resp) =
  case eitherDecode (responseBody resp) :: Either String ApiErrorResponse of
    Right apiErr ->
      TmdbError
        TmdbApiError
          { httpStatus = HTTP.statusCode (responseStatusCode resp),
            statusCode = apiErr.statusCode,
            statusMessage = apiErr.statusMessage
          }
    Left _ -> ServantError err
fromClientError err = ServantError err

newtype MovieId = MovieId {unMovieId :: Int64}
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (FromJSON, FromHttpApiData, ToHttpApiData)

newtype TvShowId = TvShowId {unTvShowId :: Int64}
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (FromJSON, FromHttpApiData, ToHttpApiData)

newtype TmdbLocale = TmdbLocale {unTmdbLocale :: Text}
  deriving stock (Show, Eq)
  deriving newtype (ToHttpApiData)

data PaginatedResponse a = PaginatedResponse
  { page :: Int64,
    results :: [a],
    totalPages :: Int64,
    totalResults :: Int64
  }
  deriving stock (Show, Eq, Generic)

instance (FromJSON a) => FromJSON (PaginatedResponse a) where
  parseJSON = withObject "PaginatedResponse" $ \o ->
    PaginatedResponse
      <$> o .: "page"
      <*> o .: "results"
      <*> o .: "total_pages"
      <*> o .: "total_results"

data PosterSize = PosterW500
  deriving stock (Show, Eq, Ord, Enum, Bounded)

posterUrl :: PosterSize -> Text -> Text
posterUrl PosterW500 path = "https://image.tmdb.org/t/p/w500" <> ensureLeadingSlash path

ensureLeadingSlash :: Text -> Text
ensureLeadingSlash path
  | T.null path = path
  | T.head path == '/' = path
  | otherwise = "/" <> path

data MovieDetail = MovieDetail
  { id :: MovieId,
    title :: Text,
    originalTitle :: Text,
    posterPath :: Maybe Text,
    releaseDate :: Maybe Day
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON MovieDetail where
  parseJSON = withObject "MovieDetail" $ \o ->
    MovieDetail
      <$> o .: "id"
      <*> o .: "title"
      <*> o .: "original_title"
      <*> o .:? "poster_path"
      <*> parseOptionalDate o "release_date"

data MediaType = MediaMovie | MediaTv | MediaPerson | MediaUnknown Text
  deriving stock (Show, Eq, Generic)

instance FromJSON MediaType where
  parseJSON = withText "MediaType" $ \case
    "movie" -> pure MediaMovie
    "tv" -> pure MediaTv
    "person" -> pure MediaPerson
    other -> pure (MediaUnknown other)

data MultiSearchResult = MultiSearchResult
  { id :: Int64,
    mediaType :: MediaType,
    name :: Maybe Text,
    title :: Maybe Text,
    originalName :: Maybe Text,
    originalTitle :: Maybe Text,
    posterPath :: Maybe Text,
    firstAirDate :: Maybe Day,
    releaseDate :: Maybe Day
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON MultiSearchResult where
  parseJSON = withObject "MultiSearchResult" $ \o ->
    MultiSearchResult
      <$> o .: "id"
      <*> o .: "media_type"
      <*> o .:? "name"
      <*> o .:? "title"
      <*> o .:? "original_name"
      <*> o .:? "original_title"
      <*> o .:? "poster_path"
      <*> parseOptionalDate o "first_air_date"
      <*> parseOptionalDate o "release_date"

data TvDetail = TvDetail
  { id :: TvShowId,
    name :: Text,
    originalName :: Text,
    posterPath :: Maybe Text,
    firstAirDate :: Maybe Day,
    numberOfSeasons :: Int64,
    numberOfEpisodes :: Int64
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON TvDetail where
  parseJSON = withObject "TvDetail" $ \o ->
    TvDetail
      <$> o .: "id"
      <*> o .: "name"
      <*> o .: "original_name"
      <*> o .:? "poster_path"
      <*> parseOptionalDate o "first_air_date"
      <*> o .: "number_of_seasons"
      <*> o .: "number_of_episodes"

parseOptionalDate :: Object -> Key -> Parser (Maybe Day)
parseOptionalDate o key = do
  mv <- o .:? key
  case mv of
    Nothing -> pure Nothing
    Just Null -> pure Nothing
    Just (String t) | T.null (T.strip t) -> pure Nothing
    Just v -> pure (parseMaybe parseJSON v)
