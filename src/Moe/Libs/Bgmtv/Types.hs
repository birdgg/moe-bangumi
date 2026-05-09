module Moe.Libs.Bgmtv.Types
  ( BgmtvError (..),
    ApiError (..),
    ErrorDetails (..),
    Response,
    fromClientError,
    SubjectId (..),
    EpisodeId (..),
    SubjectType (..),
    EpisodeType (..),
    Platform (..),
    SearchRequest (..),
    SearchFilter (..),
    SearchResponse (..),
    SubjectImages (..),
    Subject (..),
    SubjectDetail (..),
    Episode (..),
    EpisodesResponse (..),
  )
where

import Data.Aeson (FromJSON (..), Key, ToJSON (..), Value (..), eitherDecode, object, withObject, withScientific, withText, (.:), (.:?), (.!=), (.=))
import Data.Aeson.Types (Object, Parser)
import Data.Text qualified as T
import Data.Time.Calendar (Day)
import Moe.Prelude
import Servant.API (FromHttpApiData, ToHttpApiData)
import Servant.Client (ClientError (..))
import Servant.Client.Core (ResponseF (..))

data ErrorDetails = ErrorDetails
  { path :: Text,
    method :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data ApiError = ApiError
  { title :: Text,
    details :: ErrorDetails,
    requestId :: Text,
    description :: Text
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON ApiError where
  parseJSON = withObject "ApiError" $ \o ->
    ApiError
      <$> o .: "title"
      <*> o .: "details"
      <*> o .: "request_id"
      <*> o .: "description"

data BgmtvError
  = ServantError ClientError
  | BgmtvApiError ApiError
  deriving stock (Show, Generic)

type Response a = Either BgmtvError a

fromClientError :: ClientError -> BgmtvError
fromClientError err@(FailureResponse _req resp) =
  case eitherDecode (responseBody resp) of
    Right apiErr -> BgmtvApiError apiErr
    Left _ -> ServantError err
fromClientError err = ServantError err

newtype SubjectId = SubjectId {unSubjectId :: Int64}
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (FromJSON, ToJSON, FromHttpApiData, ToHttpApiData)

newtype EpisodeId = EpisodeId {unEpisodeId :: Int64}
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (FromJSON, ToJSON, FromHttpApiData, ToHttpApiData)

data SubjectType
  = Book
  | Anime
  | Music
  | Game
  | Real
  deriving stock (Show, Eq, Generic)

instance ToJSON SubjectType where
  toJSON = \case
    Book -> Number 1
    Anime -> Number 2
    Music -> Number 3
    Game -> Number 4
    Real -> Number 6

instance FromJSON SubjectType where
  parseJSON = withScientific "SubjectType" $ \n ->
    case round n :: Int of
      1 -> pure Book
      2 -> pure Anime
      3 -> pure Music
      4 -> pure Game
      6 -> pure Real
      _ -> fail $ "Unknown SubjectType: " <> show n

data EpisodeType
  = Main
  | Special
  | Opening
  | Ending
  deriving stock (Show, Eq, Generic)

instance FromJSON EpisodeType where
  parseJSON = withScientific "EpisodeType" $ \n ->
    case round n :: Int of
      0 -> pure Main
      1 -> pure Special
      2 -> pure Opening
      3 -> pure Ending
      _ -> fail $ "Unknown EpisodeType: " <> show n

data Platform
  = TV
  | Theatrical
  | OVA
  | OtherPlatform Text
  deriving stock (Show, Eq, Generic)

instance FromJSON Platform where
  parseJSON = withText "Platform" $ \t ->
    pure $ case t of
      "TV" -> TV
      "剧场版" -> Theatrical
      "OVA" -> OVA
      other -> OtherPlatform other

data SearchRequest = SearchRequest
  { keyword :: Text,
    filter_ :: Maybe SearchFilter
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON SearchRequest where
  toJSON req =
    object $
      ["keyword" .= req.keyword]
        <> maybe [] (\f -> ["filter" .= f]) req.filter_

data SearchFilter = SearchFilter
  { subjectType :: Maybe [SubjectType],
    metaTags :: Maybe [Text],
    airDate :: Maybe [Text]
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON SearchFilter where
  toJSON f =
    object $
      catMaybes
        [ ("type" .=) <$> f.subjectType,
          ("meta_tags" .=) <$> f.metaTags,
          ("air_date" .=) <$> f.airDate
        ]

data SearchResponse = SearchResponse
  { total :: Int64,
    limit :: Int64,
    offset :: Int64,
    data_ :: [Subject]
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON SearchResponse where
  parseJSON = withObject "SearchResponse" $ \o ->
    SearchResponse
      <$> o .: "total"
      <*> o .: "limit"
      <*> o .: "offset"
      <*> o .: "data"

data SubjectImages = SubjectImages
  { small :: Text,
    grid :: Text,
    large :: Text,
    medium :: Text,
    common :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data Subject = Subject
  { id :: SubjectId,
    name :: Text,
    nameCn :: Text,
    date :: Maybe Day,
    platform :: Platform,
    images :: SubjectImages,
    image :: Maybe Text,
    eps :: Int64
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON Subject where
  parseJSON = withObject "Subject" $ \o ->
    Subject
      <$> o .: "id"
      <*> o .: "name"
      <*> o .: "name_cn"
      <*> parseOptionalDay o "date"
      <*> o .: "platform"
      <*> o .: "images"
      <*> o .:? "image"
      <*> o .: "eps"

data SubjectDetail = SubjectDetail
  { id :: SubjectId,
    subjectType :: SubjectType,
    name :: Text,
    nameCn :: Text,
    summary :: Text,
    nsfw :: Bool,
    locked :: Bool,
    date :: Maybe Day,
    platform :: Platform,
    images :: SubjectImages,
    volumes :: Int64,
    eps :: Int64,
    totalEpisodes :: Int64,
    metaTags :: [Text],
    series :: Bool
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON SubjectDetail where
  parseJSON = withObject "SubjectDetail" $ \o ->
    SubjectDetail
      <$> o .: "id"
      <*> o .: "type"
      <*> o .: "name"
      <*> o .: "name_cn"
      <*> o .: "summary"
      <*> o .: "nsfw"
      <*> o .: "locked"
      <*> parseOptionalDay o "date"
      <*> o .: "platform"
      <*> o .: "images"
      <*> o .: "volumes"
      <*> o .: "eps"
      <*> o .: "total_episodes"
      <*> o .:? "meta_tags" .!= []
      <*> o .: "series"

data Episode = Episode
  { id :: EpisodeId,
    episodeType :: EpisodeType,
    name :: Text,
    nameCn :: Text,
    sort :: Double,
    ep :: Maybe Double,
    airdate :: Maybe Day
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON Episode where
  parseJSON = withObject "Episode" $ \o ->
    Episode
      <$> o .: "id"
      <*> o .: "type"
      <*> o .: "name"
      <*> o .: "name_cn"
      <*> o .: "sort"
      <*> o .:? "ep"
      <*> o .:? "airdate"

data EpisodesResponse = EpisodesResponse
  { data_ :: [Episode],
    total :: Int64,
    limit :: Int64,
    offset :: Int64
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON EpisodesResponse where
  parseJSON = withObject "EpisodesResponse" $ \o ->
    EpisodesResponse
      <$> o .: "data"
      <*> o .: "total"
      <*> o .: "limit"
      <*> o .: "offset"

parseOptionalDay :: Object -> Key -> Parser (Maybe Day)
parseOptionalDay o key = do
  mval <- o .:? key
  case mval of
    Nothing -> pure Nothing
    Just t
      | T.null t || t == "0000-00-00" -> pure Nothing
      | otherwise -> Just <$> o .: key
