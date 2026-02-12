module Moe.Web.API.DTO.Tracking
  ( TrackingResponse (..),
    TrackingWithBangumiResponse (..),
    CreateTrackingRequest (..),
    UpdateTrackingRequest (..),
    toTrackingResponse,
    toTrackingWithBangumiResponse,
    fromCreateRequest,
    applyUpdateRequest,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToSchema)
import Data.Time (UTCTime)
import Moe.Domain.Bangumi qualified as Bangumi
import Moe.Domain.Rss (PubDate)
import Moe.Domain.Shared.Entity (Entity (..), Id (..))
import Moe.Domain.Tracking (Tracking (..), TrackingType (..))
import Moe.Prelude
import Moe.Web.API.DTO.Bangumi (BangumiResponse, toBangumiResponse)

data TrackingResponse = TrackingResponse
  { id :: Int64,
    bangumiId :: Int64,
    trackingType :: TrackingType,
    rssUrl :: Maybe Text,
    rssEnabled :: Bool,
    lastPubdate :: Maybe PubDate,
    currentEpisode :: Word32,
    episodeOffset :: Word32,
    isBDrip :: Bool,
    autoComplete :: Bool,
    createdAt :: UTCTime
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, ToSchema)

data CreateTrackingRequest = CreateTrackingRequest
  { bangumiId :: Int64,
    trackingType :: TrackingType,
    mikanId :: Maybe Word32,
    rssUrl :: Maybe Text,
    currentEpisode :: Maybe Word32,
    episodeOffset :: Maybe Word32
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToSchema)

data UpdateTrackingRequest = UpdateTrackingRequest
  { trackingType :: Maybe TrackingType,
    rssUrl :: Maybe Text,
    rssEnabled :: Maybe Bool,
    currentEpisode :: Maybe Word32,
    episodeOffset :: Maybe Word32,
    autoComplete :: Maybe Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToSchema)

{- HLINT ignore toTrackingResponse "Redundant id" -}
toTrackingResponse :: Entity Tracking -> TrackingResponse
toTrackingResponse entity =
  let t = entity.entityVal
   in TrackingResponse
        { id = coerce entity.entityId,
          bangumiId = coerce t.bangumiId,
          trackingType = t.trackingType,
          rssUrl = t.rssUrl,
          rssEnabled = t.rssEnabled,
          lastPubdate = t.lastPubdate,
          currentEpisode = t.currentEpisode,
          episodeOffset = t.episodeOffset,
          isBDrip = t.isBDrip,
          autoComplete = t.autoComplete,
          createdAt = entity.createdAt
        }

-- | Convert a create request to a Tracking domain model.
-- The episodeOffset parameter is the auto-detected offset from metadata,
-- used as fallback when the request does not specify one.
fromCreateRequest :: Word32 -> CreateTrackingRequest -> Tracking
fromCreateRequest autoOffset req =
  Tracking
    { bangumiId = Id req.bangumiId,
      trackingType = req.trackingType,
      rssUrl = req.rssUrl <|> (mikanIdToRssUrl <$> req.mikanId),
      rssEnabled = True,
      lastPubdate = Nothing,
      currentEpisode = fromMaybe 0 req.currentEpisode,
      episodeOffset = fromMaybe autoOffset req.episodeOffset,
      isBDrip = False,
      autoComplete = True
    }

mikanIdToRssUrl :: Word32 -> Text
mikanIdToRssUrl mid = "https://mikanani.me/RSS/Bangumi?bangumiId=" <> show mid

applyUpdateRequest :: UpdateTrackingRequest -> Entity Tracking -> Entity Tracking
applyUpdateRequest req entity =
  let t = entity.entityVal
      updated =
        Tracking
          { bangumiId = t.bangumiId,
            trackingType = fromMaybe t.trackingType req.trackingType,
            rssUrl = req.rssUrl <|> t.rssUrl,
            rssEnabled = fromMaybe t.rssEnabled req.rssEnabled,
            lastPubdate = t.lastPubdate,
            currentEpisode = fromMaybe t.currentEpisode req.currentEpisode,
            episodeOffset = fromMaybe t.episodeOffset req.episodeOffset,
            isBDrip = t.isBDrip,
            autoComplete = fromMaybe t.autoComplete req.autoComplete
          }
   in entity {entityVal = updated}

data TrackingWithBangumiResponse = TrackingWithBangumiResponse
  { tracking :: TrackingResponse,
    bangumi :: BangumiResponse
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, ToSchema)

toTrackingWithBangumiResponse :: Entity Tracking -> Entity Bangumi.Bangumi -> TrackingWithBangumiResponse
toTrackingWithBangumiResponse t b =
  TrackingWithBangumiResponse
    { tracking = toTrackingResponse t,
      bangumi = toBangumiResponse b
    }
