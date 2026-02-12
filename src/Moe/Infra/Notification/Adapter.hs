-- | Telegram interpreter for Notification effect.
module Moe.Infra.Notification.Adapter
  ( -- * Interpreter
    runNotificationDynamic,

    -- * Re-exports
    module Moe.Infra.Notification.Effect,
  )
where

import Data.Text.Display (display)
import Effectful
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Log qualified as Log
import Moe.Domain.Setting (NotificationConfig (..), UserPreference (..))
import Moe.Infra.Notification.Client qualified as Client
import Moe.Infra.Notification.Effect
import Moe.Infra.Setting.Effect (Setting, getSetting)
import Moe.Prelude
import Network.HTTP.Client (Manager)

-- | Dynamic Notification interpreter that reads config from Setting on each call.
-- Silently skips when notification config is not set.
-- Logs errors but does not propagate them.
runNotificationDynamic ::
  (Setting :> es, Log :> es, IOE :> es) =>
  Manager ->
  Eff (Notification : es) a ->
  Eff es a
runNotificationDynamic manager = interpret $ \_ -> \case
  SendNotification message mImageUrl -> do
    pref <- getSetting
    let cfg = pref.notification
    if cfg.botToken == ""
      then pass
      else do
        result <- case mImageUrl of
          Nothing -> Client.sendTelegramMessage manager cfg.botToken cfg.chatId message
          Just imageUrl -> Client.sendTelegramPhoto manager cfg.botToken cfg.chatId message imageUrl
        case result of
          Left err -> Log.logAttention_ $ display err
          Right () -> pass
