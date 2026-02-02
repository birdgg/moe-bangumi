module Moe.Domain.Tracking.Types
  ( TrackingId (..),
    TrackingType (..),
    Tracking (..),
  )
where

import Data.Int (Int64)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Word (Word32)
import GHC.Generics (Generic)
import Moe.Domain.Bangumi.Types (BangumiId)
import Relude (ToText (..))

newtype TrackingId = TrackingId Int64
  deriving stock (Eq, Show)

data TrackingType = Subscription | Collection
  deriving stock (Eq, Show, Ord, Enum, Bounded, Generic)

instance ToText TrackingType where
  toText Subscription = "subscription"
  toText Collection = "collection"

data Tracking = Tracking
  { id :: Maybe TrackingId,
    bangumiId :: BangumiId,
    trackingType :: TrackingType,
    rssUrl :: Maybe Text,
    rssEnabled :: Bool,
    lastPubdate :: Maybe UTCTime,
    currentEpisode :: Word32,
    createdAt :: Maybe UTCTime
  }
  deriving stock (Eq, Show)
