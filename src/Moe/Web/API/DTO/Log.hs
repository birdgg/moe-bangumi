-- | DTOs for log query API.
module Moe.Web.API.DTO.Log
  ( LogEntry (..),
    LogsResponse (..),
  )
where

import Data.Aeson (FromJSON (..), ToJSON, (.:), (.:?), withObject)
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KM
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
  deriving anyclass (ToJSON)

instance FromJSON LogEntry where
  parseJSON = withObject "LogEntry" $ \obj -> do
    time <- obj .: "time"
    level <- obj .: "level"
    component <- obj .:? "component" <&> fromMaybe ""
    baseMessage <- obj .:? "message" <&> fromMaybe ""
    let message = case KM.lookup "data" obj of
          Just (Aeson.Object d) | not (KM.null d) ->
            baseMessage <> " " <> decodeUtf8 (toStrict (Aeson.encode (Aeson.Object d)))
          _ -> baseMessage
    pure LogEntry {time, level, component, message}

-- | Paginated log query response.
data LogsResponse = LogsResponse
  { entries :: [LogEntry],
    total :: Word32,
    page :: Word32,
    pageSize :: Word32
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)
