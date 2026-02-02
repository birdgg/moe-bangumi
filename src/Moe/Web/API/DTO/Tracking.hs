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

import Control.Applicative ((<|>))
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Coerce (coerce)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Conversions (ToText (..))
import Data.Time (UTCTime)
import Data.Word (Word32)
import GHC.Generics (Generic)
import Moe.Domain.Bangumi.Types (BangumiId (..))
import Moe.Domain.Bangumi.Types qualified as Bangumi
import Moe.Web.API.DTO.Bangumi (BangumiResponse, toBangumiResponse)
import Moe.Domain.Tracking.Types (Tracking (..), TrackingId (..), TrackingType (..))

instance ToJSON TrackingType where
  toJSON = toJSON . toText

instance FromJSON TrackingType where
  parseJSON = fmap parseTrackingType . parseJSON
    where
      parseTrackingType :: Text -> TrackingType
      parseTrackingType "subscription" = Subscription
      parseTrackingType "collection" = Collection
      parseTrackingType _ = Subscription

instance ToSchema TrackingType

data TrackingResponse = TrackingResponse
  { id :: Int64,
    bangumiId :: Int64,
    trackingType :: TrackingType,
    rssUrl :: Maybe Text,
    rssEnabled :: Bool,
    lastPubdate :: Maybe UTCTime,
    currentEpisode :: Word32,
    createdAt :: Maybe UTCTime
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, ToSchema)

data CreateTrackingRequest = CreateTrackingRequest
  { bangumiId :: Int64,
    trackingType :: TrackingType,
    mikanId :: Maybe Word32,
    currentEpisode :: Maybe Word32
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToSchema)

data UpdateTrackingRequest = UpdateTrackingRequest
  { trackingType :: Maybe TrackingType,
    rssUrl :: Maybe Text,
    rssEnabled :: Maybe Bool,
    currentEpisode :: Maybe Word32
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToSchema)

{- HLINT ignore toTrackingResponse "Redundant id" -}
toTrackingResponse :: Tracking -> Maybe TrackingResponse
toTrackingResponse t = do
  tid <- t.id
  pure
    TrackingResponse
      { id = coerce tid,
        bangumiId = coerce t.bangumiId,
        trackingType = t.trackingType,
        rssUrl = t.rssUrl,
        rssEnabled = t.rssEnabled,
        lastPubdate = t.lastPubdate,
        currentEpisode = t.currentEpisode,
        createdAt = t.createdAt
      }

fromCreateRequest :: CreateTrackingRequest -> Tracking
fromCreateRequest req =
  Tracking
    { id = Nothing,
      bangumiId = BangumiId req.bangumiId,
      trackingType = req.trackingType,
      rssUrl = mikanIdToRssUrl <$> req.mikanId,
      rssEnabled = True,
      lastPubdate = Nothing,
      currentEpisode = fromMaybe 0 req.currentEpisode,
      createdAt = Nothing
    }

mikanIdToRssUrl :: Word32 -> Text
mikanIdToRssUrl mid = "https://mikanani.me/RSS/Bangumi?bangumiId=" <> T.pack (show mid)

applyUpdateRequest :: UpdateTrackingRequest -> Tracking -> Tracking
applyUpdateRequest req t =
  Tracking
    { id = t.id,
      bangumiId = t.bangumiId,
      trackingType = fromMaybe t.trackingType req.trackingType,
      rssUrl = req.rssUrl <|> t.rssUrl,
      rssEnabled = fromMaybe t.rssEnabled req.rssEnabled,
      lastPubdate = t.lastPubdate,
      currentEpisode = fromMaybe t.currentEpisode req.currentEpisode,
      createdAt = t.createdAt
    }

data TrackingWithBangumiResponse = TrackingWithBangumiResponse
  { tracking :: TrackingResponse,
    bangumi :: BangumiResponse
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, ToSchema)

toTrackingWithBangumiResponse :: Tracking -> Bangumi.Bangumi -> Maybe TrackingWithBangumiResponse
toTrackingWithBangumiResponse t b = do
  trackingResp <- toTrackingResponse t
  pure
    TrackingWithBangumiResponse
      { tracking = trackingResp,
        bangumi = toBangumiResponse b
      }
