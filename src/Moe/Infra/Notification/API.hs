-- | Telegram Bot API request types.
module Moe.Infra.Notification.API
  ( SendMessageRequest (..),
    SendPhotoRequest (..),
  )
where

import Data.Aeson (ToJSON (..), object, (.=))
import Moe.Prelude

-- | Request body for sendMessage.
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

-- | Request body for sendPhoto.
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
