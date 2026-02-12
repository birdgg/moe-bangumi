module Moe.Web.API.DTO.Bangumi
  ( BangumiResponse (..),
    TmdbSearchResult (..),
    UpdateBangumiTmdbIdRequest (..),
    toBangumiResponse,
    toTmdbSearchResult,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToSchema)
import Data.Time (UTCTime)
import Data.Time.Calendar (Day)
import Moe.Domain.Bangumi (BangumiKind, SeasonNumber, extractYear)
import Moe.Domain.Bangumi qualified as Types
import Moe.Domain.Shared.Entity (Entity (..), Id (..))
import Moe.Prelude

data BangumiResponse = BangumiResponse
  { id :: Int64,
    titleChs :: Text,
    titleJap :: Maybe Text,
    airDate :: Day,
    season :: Maybe SeasonNumber,
    kind :: BangumiKind,
    posterUrl :: Maybe Text,
    totalEpisodes :: Maybe Int,
    tmdbId :: Maybe Word32,
    mikanId :: Maybe Word32,
    bgmtvId :: Maybe Word32,
    createdAt :: UTCTime
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, ToSchema)

{- HLINT ignore toBangumiResponse "Redundant id" -}
toBangumiResponse :: Entity Types.Bangumi -> BangumiResponse
toBangumiResponse entity =
  let b = entity.entityVal
   in BangumiResponse
        { id = coerce entity.entityId,
          titleChs = b.titleChs,
          titleJap = b.titleJap,
          airDate = b.airDate,
          season = b.season,
          kind = b.kind,
          posterUrl = b.posterUrl,
          totalEpisodes = b.totalEpisodes,
          tmdbId = coerce <$> b.tmdbId,
          mikanId = coerce <$> b.mikanId,
          bgmtvId = coerce <$> b.bgmtvId,
          createdAt = entity.createdAt
        }

-- | Lightweight TMDB search result for the selector dropdown.
data TmdbSearchResult = TmdbSearchResult
  { tmdbId :: Word32,
    title :: Text,
    year :: Maybe Int,
    mediaType :: Text,
    posterUrl :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, ToSchema)

-- | Convert domain Bangumi to TmdbSearchResult.
toTmdbSearchResult :: Types.Bangumi -> Maybe TmdbSearchResult
toTmdbSearchResult b = do
  Types.TmdbId tid <- b.tmdbId
  pure
    TmdbSearchResult
      { tmdbId = tid,
        title = b.titleChs,
        year = Just $ fromIntegral $ extractYear b.airDate,
        mediaType = toText b.kind,
        posterUrl = b.posterUrl
      }

-- | Request to update a bangumi's TMDB ID.
data UpdateBangumiTmdbIdRequest = UpdateBangumiTmdbIdRequest
  { tmdbId :: Maybe Word32
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToSchema)
