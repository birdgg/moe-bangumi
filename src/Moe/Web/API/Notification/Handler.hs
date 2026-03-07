-- | Handler for testing Telegram bot connection.
module Moe.Web.API.Notification.Handler
  ( handleTestNotification,
  )
where

import Data.Text.Display (display)
import Moe.Infra.Http.Effect (getHttpManager)
import Moe.Infra.Notification.Client (sendTelegramMessage)
import Moe.Prelude
import Moe.Web.API.DTO.Notification (TestNotificationRequest (..), TestNotificationResponse (..))
import Moe.Web.Types (ServerEff)

-- | Test a Telegram bot connection by sending a test message.
handleTestNotification :: TestNotificationRequest -> ServerEff TestNotificationResponse
handleTestNotification req = do
  mgr <- getHttpManager
  result <- sendTelegramMessage mgr req.botToken req.chatId "Moe Bangumi test notification"
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
