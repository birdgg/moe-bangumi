-- | Telegram Bot API HTTP client.
--
-- Pure IO functions for sending messages and photos via the Telegram Bot API.
-- Uses http-client Manager for connection pooling.
module Moe.Infrastructure.Notification.Client
  ( sendTelegramMessage,
    sendTelegramPhoto,
  )
where

import Control.Exception (try)
import Data.Aeson (encode, object, (.=))
import Data.ByteString.Lazy qualified as LBS
import Moe.Prelude
import Network.HTTP.Client
import Network.HTTP.Types.Status qualified as Status

-- | Send a text message via Telegram Bot API.
sendTelegramMessage :: Manager -> Text -> Text -> Text -> IO (Either Text ())
sendTelegramMessage manager botToken chatId message =
  let url = "https://api.telegram.org/bot" <> botToken <> "/sendMessage"
      body =
        encode $
          object
            [ "chat_id" .= chatId,
              "text" .= message,
              "parse_mode" .= ("HTML" :: Text)
            ]
   in sendTelegramRequest manager url body

-- | Send a photo with caption via Telegram Bot API.
sendTelegramPhoto :: Manager -> Text -> Text -> Text -> Text -> IO (Either Text ())
sendTelegramPhoto manager botToken chatId message imageUrl =
  let url = "https://api.telegram.org/bot" <> botToken <> "/sendPhoto"
      body =
        encode $
          object
            [ "chat_id" .= chatId,
              "photo" .= imageUrl,
              "caption" .= message,
              "parse_mode" .= ("HTML" :: Text)
            ]
   in sendTelegramRequest manager url body

-- | Send a request to the Telegram Bot API.
sendTelegramRequest :: Manager -> Text -> LBS.ByteString -> IO (Either Text ())
sendTelegramRequest manager url body = do
  result <- try $ do
    request <- parseRequest (toString url)
    let request' =
          request
            { method = "POST",
              requestBody = RequestBodyLBS body,
              requestHeaders = [("Content-Type", "application/json")]
            }
    response <- httpLbs request' manager
    pure $ Status.statusCode (responseStatus response)
  case result of
    Left (e :: SomeException) ->
      pure $ Left $ "Telegram request failed: " <> show e
    Right status
      | status >= 200 && status < 300 ->
          pure $ Right ()
      | otherwise ->
          pure $ Left $ "Telegram API returned HTTP " <> show status
