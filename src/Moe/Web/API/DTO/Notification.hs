-- | DTOs for notification test endpoint.
module Moe.Web.API.DTO.Notification
  ( TestNotificationRequest (..),
    TestNotificationResponse (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToSchema)
import Moe.Prelude

-- | Request to test a Telegram bot connection.
data TestNotificationRequest = TestNotificationRequest
  { botToken :: Text,
    chatId :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToSchema)

-- | Response from a Telegram bot connection test.
data TestNotificationResponse = TestNotificationResponse
  { success :: Bool,
    message :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, ToSchema)
