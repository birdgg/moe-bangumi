-- | DTOs for log query API.
module Moe.Web.API.DTO.Log
  ( LogEntry (..),
    LogsResponse (..),
  )
where

import Data.Aeson (FromJSON (..), ToJSON, (.:), (.:?), withObject)
import Data.OpenApi (ToSchema)
import Data.Time (UTCTime)
import Moe.Prelude

-- | A single log entry parsed from a JSON log line.
data LogEntry = LogEntry
  { time :: UTCTime,
    level :: Text,
    component :: Text,
    message :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, ToSchema)

instance FromJSON LogEntry where
  parseJSON = withObject "LogEntry" $ \obj -> do
    time <- obj .: "time"
    level <- obj .: "level"
    component <- obj .:? "component" <&> fromMaybe ""
    message <- obj .:? "message" <&> fromMaybe ""
    pure LogEntry {time, level, component, message}

-- | Paginated log query response.
data LogsResponse = LogsResponse
  { entries :: [LogEntry],
    total :: Word32,
    page :: Word32,
    pageSize :: Word32
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, ToSchema)
