-- | Telegram interpreter for Notification effect.
module Moe.Infrastructure.Notification.Adapter
  ( -- * Interpreter
    runNotificationDynamic,

    -- * Re-exports
    module Moe.Infrastructure.Notification.Effect,
  )
where

import Effectful
import Effectful.Dispatch.Dynamic (interpret)
import Moe.Domain.Setting.Types (NotificationConfig (..), UserPreference (..))
import Moe.Infrastructure.Notification.Client qualified as Client
import Moe.Infrastructure.Notification.Effect
import Moe.Infrastructure.Setting.Effect (Setting, getSetting)
import Moe.Prelude
import Network.HTTP.Client (Manager)

-- | Dynamic Notification interpreter that reads config from Setting on each call.
-- Silently skips when notification config is not set.
runNotificationDynamic ::
  (Setting :> es, IOE :> es) =>
  Manager ->
  Eff (Notification : es) a ->
  Eff es a
runNotificationDynamic manager = interpret $ \_ -> \case
  SendNotification message mImageUrl -> do
    pref <- getSetting
    case pref.notification of
      Nothing -> pass
      Just cfg -> do
        result <- liftIO $ case mImageUrl of
          Nothing -> Client.sendTelegramMessage manager cfg.botToken cfg.chatId message
          Just imageUrl -> Client.sendTelegramPhoto manager cfg.botToken cfg.chatId message imageUrl
        case result of
          Left _err -> pass
          Right () -> pass
