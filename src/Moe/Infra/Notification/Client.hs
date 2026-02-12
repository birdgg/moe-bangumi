-- | Telegram Bot API client.
--
-- Direct HTTP client for sending messages and photos via Telegram Bot API.
module Moe.Infra.Notification.Client
  ( sendTelegramMessage,
    sendTelegramPhoto,
  )
where

import Data.Aeson (encode)
import Effectful ((:>))
import Moe.Infra.Notification.API (SendMessageRequest (..), SendPhotoRequest (..))
import Moe.Infra.Notification.Types (NotificationSendError (..))
import Moe.Prelude
import Control.Exception (try)
import Network.HTTP.Client (Manager, HttpException, RequestBody (..), httpLbs, method, parseRequest, requestBody, requestHeaders, responseStatus)
import Network.HTTP.Types.Status (statusCode, statusIsSuccessful)

-- | Build a Telegram Bot API URL.
buildUrl :: Text -> Text -> String
buildUrl botToken endpoint =
  toString ("https://api.telegram.org/bot" <> botToken <> "/" <> endpoint)

-- | Send a POST request to Telegram Bot API.
telegramPost :: (IOE :> es) => Manager -> Text -> Text -> LByteString -> Eff es (Either NotificationSendError ())
telegramPost manager botToken endpoint body = do
  result <- liftIO $ try @HttpException $ do
    req <- parseRequest (buildUrl botToken endpoint)
    let req' =
          req
            { method = "POST",
              requestBody = RequestBodyLBS body,
              requestHeaders = [("Content-Type", "application/json")]
            }
    httpLbs req' manager
  pure $ case result of
    Left err ->
      Left (NtfNetworkError (show err))
    Right response
      | statusIsSuccessful (responseStatus response) -> Right ()
      | otherwise ->
          Left (NtfHttpError (statusCode (responseStatus response)))

-- | Send a text message via Telegram Bot API.
sendTelegramMessage :: (IOE :> es) => Manager -> Text -> Text -> Text -> Eff es (Either NotificationSendError ())
sendTelegramMessage manager botToken chatId message =
  telegramPost manager botToken "sendMessage" (encode SendMessageRequest{chatId, text = message})

-- | Send a photo with caption via Telegram Bot API.
sendTelegramPhoto :: (IOE :> es) => Manager -> Text -> Text -> Text -> Text -> Eff es (Either NotificationSendError ())
sendTelegramPhoto manager botToken chatId message imageUrl =
  telegramPost manager botToken "sendPhoto" (encode SendPhotoRequest{chatId, photo = imageUrl, caption = message})
