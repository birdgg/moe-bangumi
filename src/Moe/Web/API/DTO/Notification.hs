-- | DTOs for notification test endpoint.
module Moe.Web.API.DTO.Notification
  ( TestNotificationRequest (..),
    TestNotificationResponse (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Moe.Prelude

-- | Request to test a notification provider connection.
data TestNotificationRequest = TestNotificationRequest
  { provider :: Text,
    botToken :: Maybe Text,
    chatId :: Maybe Text,
    webhookUrl :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

-- | Response from a notification provider connection test.
data TestNotificationResponse = TestNotificationResponse
  { success :: Bool,
    message :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)
