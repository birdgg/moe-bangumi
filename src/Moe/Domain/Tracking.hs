module Moe.Domain.Tracking
  ( TrackingId,
    TrackingType (..),
    Tracking (..),
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), withText)
import Data.OpenApi (NamedSchema (..), OpenApiType (..), Schema (..), ToSchema (..))
import Effectful.Sqlite (FromField (..), FromRow, ToField (..), ToRow)
import Moe.Domain.Bangumi (BangumiId)
import Moe.Domain.Rss (PubDate)
import Moe.Domain.Shared.Entity (Id)
import Moe.Prelude

type TrackingId = Id Tracking

data TrackingType = Subscription | Collection
  deriving stock (Eq, Show, Ord, Enum, Bounded, Generic)

instance ToText TrackingType where
  toText Subscription = "subscription"
  toText Collection = "collection"

instance FromText TrackingType

instance ToJSON TrackingType where
  toJSON = toJSON . toText

instance FromJSON TrackingType where
  parseJSON = withText "TrackingType" $ \case
    "subscription" -> pure Subscription
    "collection" -> pure Collection
    other -> fail $ "Unknown TrackingType: " <> toString other

instance FromField TrackingType where
  fromField f = do
    t <- fromField @Text f
    case t of
      "subscription" -> pure Subscription
      "collection" -> pure Collection
      _ -> fail $ "Invalid tracking type: " <> toString t

instance ToField TrackingType where
  toField = toField . toText

instance ToSchema TrackingType where
  declareNamedSchema _ = do
    let enumValues = map toJSON [minBound @TrackingType .. maxBound]
    pure $
      NamedSchema (Just "TrackingType") $
        mempty
          { _schemaType = Just OpenApiString,
            _schemaEnum = Just enumValues
          }

data Tracking = Tracking
  { bangumiId :: BangumiId,
    trackingType :: TrackingType,
    rssUrl :: Maybe Text,
    rssEnabled :: Bool,
    lastPubdate :: Maybe PubDate,
    currentEpisode :: Word32,
    episodeOffset :: Word32,
    isBDrip :: Bool,
    autoComplete :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, FromRow, ToRow)
