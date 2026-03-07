-- | Handler for testing notification provider connections.
module Moe.Web.API.Notification.Handler
  ( handleTestNotification,
  )
where

import Data.Text.Display (display)
import Moe.Infra.Notification.Discord.API (sendDiscordMessage)
import Moe.Infra.Notification.Telegram.API (sendTelegramMessage)
import Moe.Infra.Notification.Types (NotificationError (..))
import Moe.Prelude
import Moe.Web.API.DTO.Notification (TestNotificationRequest (..), TestNotificationResponse (..))
import Moe.Web.Types (ServerEff)

-- | Test a notification provider connection by sending a test message.
handleTestNotification :: TestNotificationRequest -> ServerEff TestNotificationResponse
handleTestNotification req = do
  result <- case req.provider of
    "telegram" -> do
      let botToken = fromMaybe "" req.botToken
          chatId = fromMaybe "" req.chatId
      if botToken == "" || chatId == ""
        then pure $ Left $ ConfigError "Bot Token and Chat ID are required"
        else sendTelegramMessage botToken chatId "Moe Bangumi test notification"
    "discord" -> do
      let webhookUrl = fromMaybe "" req.webhookUrl
      if webhookUrl == ""
        then pure $ Left $ ConfigError "Webhook URL is required"
        else sendDiscordMessage webhookUrl "Moe Bangumi test notification"
    other ->
      pure $ Left $ ConfigError $ "Unknown provider: " <> other
  pure $ case result of
    Left err ->
      TestNotificationResponse
        { success = False,
          message = display err
        }
    Right () ->
      TestNotificationResponse
        { success = True,
          message = "Message sent"
        }
