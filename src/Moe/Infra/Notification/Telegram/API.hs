-- | Telegram Bot API client using Servant.
module Moe.Infra.Notification.Telegram.API
  ( sendTelegramMessage,
    sendTelegramPhoto,
  )
where

import Data.Aeson (ToJSON (..), Value, object, (.=))
import Moe.Infra.Http.Effect (Http, runServantClient)
import Moe.Infra.Notification.Types (NotificationError (..))
import Moe.Prelude
import Servant.API (JSON, Post, ReqBody, type (:<|>) (..))
import Servant.API qualified as Servant
import Servant.Client (BaseUrl (..), ClientM, Scheme (..), client)

data SendMessageRequest = SendMessageRequest
  { chatId :: Text,
    text :: Text
  }

instance ToJSON SendMessageRequest where
  toJSON req =
    object
      [ "chat_id" .= req.chatId,
        "text" .= req.text,
        "parse_mode" .= ("HTML" :: Text)
      ]

data SendPhotoRequest = SendPhotoRequest
  { chatId :: Text,
    photo :: Text,
    caption :: Text
  }

instance ToJSON SendPhotoRequest where
  toJSON req =
    object
      [ "chat_id" .= req.chatId,
        "photo" .= req.photo,
        "caption" .= req.caption,
        "parse_mode" .= ("HTML" :: Text)
      ]

type TelegramAPI =
  "sendMessage" Servant.:> ReqBody '[JSON] SendMessageRequest Servant.:> Post '[JSON] Value
    :<|> "sendPhoto" Servant.:> ReqBody '[JSON] SendPhotoRequest Servant.:> Post '[JSON] Value

sendMessageClient :: SendMessageRequest -> ClientM Value
sendPhotoClient :: SendPhotoRequest -> ClientM Value
sendMessageClient :<|> sendPhotoClient = client (Proxy @TelegramAPI)

telegramBaseUrl :: Text -> BaseUrl
telegramBaseUrl botToken =
  BaseUrl
    { baseUrlScheme = Https,
      baseUrlHost = "api.telegram.org",
      baseUrlPort = 443,
      baseUrlPath = "/bot" <> toString botToken
    }

sendTelegramMessage :: (Http :> es) => Text -> Text -> Text -> Eff es (Either NotificationError ())
sendTelegramMessage botToken chatId message = do
  result <- runServantClient (telegramBaseUrl botToken) (sendMessageClient SendMessageRequest {chatId, text = message})
  pure $ bimap NetworkError (const ()) result

sendTelegramPhoto :: (Http :> es) => Text -> Text -> Text -> Text -> Eff es (Either NotificationError ())
sendTelegramPhoto botToken chatId message imageUrl = do
  result <- runServantClient (telegramBaseUrl botToken) (sendPhotoClient SendPhotoRequest {chatId, photo = imageUrl, caption = message})
  pure $ bimap NetworkError (const ()) result
