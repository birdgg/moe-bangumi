-- | Notification effect and interpreter.
module Moe.Infra.Notification.Effect
  ( -- * Effect
    Notification (..),

    -- * Operations
    sendNotification,

    -- * Interpreter
    runNotification,
  )
where

import Data.Text.Display (display)
import Effectful.Log qualified as Log
import Moe.Domain.Setting (NotificationConfig (..), UserPreference (..))
import Moe.Infra.Http.Effect (Http)
import Moe.Infra.Notification.Discord.API qualified as Discord
import Moe.Infra.Notification.Telegram.API qualified as Telegram
import Moe.Infra.Setting.Effect (Setting, getSetting)
import Moe.Prelude

-- | Abstract notification effect.
data Notification :: Effect where
  -- | Send a notification with message text and optional image URL.
  SendNotification :: Text -> Maybe Text -> Notification m ()

makeEffect ''Notification

runNotification ::
  (Http :> es, Setting :> es, Log :> es) =>
  Eff (Notification : es) a ->
  Eff es a
runNotification = interpret $ \_ -> \case
  SendNotification message mImageUrl -> do
    pref <- getSetting
    let cfg = pref.notification
    when (cfg.botToken /= "" && cfg.chatId /= "") $
      notify $ \case
        Nothing -> Telegram.sendTelegramMessage cfg.botToken cfg.chatId message
        Just imageUrl -> Telegram.sendTelegramPhoto cfg.botToken cfg.chatId message imageUrl
    when (cfg.discordWebhookUrl /= "") $
      notify $ \case
        Nothing -> Discord.sendDiscordMessage cfg.discordWebhookUrl message
        Just imageUrl -> Discord.sendDiscordEmbed cfg.discordWebhookUrl message imageUrl
   where
    notify action = do
      result <- action mImageUrl
      case result of
        Left err -> Log.logAttention_ $ display err
        Right () -> pass
