-- | Discord Webhook API client using Servant.
module Moe.Infra.Notification.Discord.API
  ( sendDiscordMessage,
    sendDiscordEmbed,
  )
where

import Data.Aeson (ToJSON (..), Value, object, (.=))
import Moe.Infra.Http.Effect (Http, runServantClient)
import Moe.Infra.Notification.Types (NotificationError (..))
import Data.List (stripPrefix)
import Moe.Prelude
import Servant.API (JSON, Post, ReqBody)
import Servant.API qualified as Servant
import Servant.Client (BaseUrl (..), ClientM, Scheme (..), client)

-- | Simple webhook message with just text content.
newtype WebhookMessage = WebhookMessage
  { content :: Text
  }

instance ToJSON WebhookMessage where
  toJSON msg = object ["content" .= msg.content]

-- | Webhook message with an embed containing an image.
data WebhookEmbed = WebhookEmbed
  { content :: Text,
    imageUrl :: Text
  }

instance ToJSON WebhookEmbed where
  toJSON msg =
    object
      [ "content" .= msg.content,
        "embeds"
          .= [ object
                 [ "image" .= object ["url" .= msg.imageUrl]
                 ]
             ]
      ]

type DiscordWebhookAPI =
  ReqBody '[JSON] Value Servant.:> Post '[JSON] Value

webhookClient :: Value -> ClientM Value
webhookClient = client (Proxy @DiscordWebhookAPI)

-- | Parse a Discord webhook URL into BaseUrl.
-- Example: https://discord.com/api/webhooks/123/abc
parseWebhookUrl :: Text -> Either NotificationError BaseUrl
parseWebhookUrl url =
  case parseUrl (toString url) of
    Just (scheme, host, port, path) ->
      Right
        BaseUrl
          { baseUrlScheme = scheme,
            baseUrlHost = host,
            baseUrlPort = port,
            baseUrlPath = path
          }
    Nothing -> Left $ ConfigError "Invalid Discord webhook URL"
  where
    parseUrl :: String -> Maybe (Scheme, String, Int, String)
    parseUrl s
      | Just rest <- stripPrefix "https://" s =
          let (hostPort, path) = span (/= '/') rest
              (host, portStr) = span (/= ':') hostPort
              port = case stripPrefix ":" portStr of
                Just p -> fromMaybe 443 (readMaybe p)
                Nothing -> 443
           in Just (Https, host, port, path)
      | Just rest <- stripPrefix "http://" s =
          let (hostPort, path) = span (/= '/') rest
              (host, portStr) = span (/= ':') hostPort
              port = case stripPrefix ":" portStr of
                Just p -> fromMaybe 80 (readMaybe p)
                Nothing -> 80
           in Just (Http, host, port, path)
      | otherwise = Nothing

sendDiscordMessage :: (Http :> es) => Text -> Text -> Eff es (Either NotificationError ())
sendDiscordMessage webhookUrl message =
  case parseWebhookUrl webhookUrl of
    Left err -> pure $ Left err
    Right baseUrl -> do
      result <- runServantClient baseUrl (webhookClient $ toJSON WebhookMessage {content = message})
      pure $ bimap NetworkError (const ()) result

sendDiscordEmbed :: (Http :> es) => Text -> Text -> Text -> Eff es (Either NotificationError ())
sendDiscordEmbed webhookUrl message imageUrl =
  case parseWebhookUrl webhookUrl of
    Left err -> pure $ Left err
    Right baseUrl -> do
      result <- runServantClient baseUrl (webhookClient $ toJSON WebhookEmbed {content = message, imageUrl})
      pure $ bimap NetworkError (const ()) result
