module Moe.Web.API.DTO.Bangumi
  ( BangumiResponse (..),
    toBangumiResponse,
  )
where

import Data.Aeson (ToJSON)
import Data.OpenApi (ToSchema)
import Data.Time (UTCTime)
import Data.Time.Calendar (Day)
import Moe.Domain.Bangumi.Types (BangumiKind, SeasonNumber)
import Moe.Domain.Bangumi.Types qualified as Types
import Moe.Prelude

data BangumiResponse = BangumiResponse
  { id :: Int64,
    titleChs :: Text,
    titleJap :: Maybe Text,
    airDate :: Maybe Day,
    season :: Maybe SeasonNumber,
    kind :: BangumiKind,
    posterUrl :: Maybe Text,
    tmdbId :: Maybe Word32,
    mikanId :: Maybe Word32,
    bgmtvId :: Maybe Word32,
    createdAt :: UTCTime
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, ToSchema)

{- HLINT ignore toBangumiResponse "Redundant id" -}
toBangumiResponse :: Types.Bangumi -> BangumiResponse
toBangumiResponse b =
  BangumiResponse
    { id = coerce b.id,
      titleChs = b.titleChs,
      titleJap = b.titleJap,
      airDate = b.airDate,
      season = b.season,
      kind = b.kind,
      posterUrl = b.posterUrl,
      tmdbId = coerce <$> b.tmdbId,
      mikanId = coerce <$> b.mikanId,
      bgmtvId = coerce <$> b.bgmtvId,
      createdAt = b.createdAt
    }
