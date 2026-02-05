-- | Telegram interpreter for Notification effect.
module Moe.Infrastructure.Notification.Adapter
  ( -- * Interpreter
    runNotificationTelegram,

    -- * Re-exports
    module Moe.Infrastructure.Notification.Effect,
  )
where

import Effectful
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Error.Static (Error, throwError)
import Moe.Domain.Setting.Types (NotificationConfig (..))
import Moe.Error (MoeError (..))
import Moe.Infrastructure.Notification.Client qualified as Client
import Moe.Infrastructure.Notification.Effect
import Moe.Prelude
import Network.HTTP.Client (Manager)

-- | Run Notification effect using Telegram Bot API.
--
-- Takes notification config and HTTP manager,
-- converting any failure to MoeError.
runNotificationTelegram ::
  (Error MoeError :> es, IOE :> es) =>
  NotificationConfig ->
  Manager ->
  Eff (Notification : es) a ->
  Eff es a
runNotificationTelegram cfg manager = interpret $ \_ -> \case
  SendNotification message mImageUrl -> do
    result <- liftIO $ case mImageUrl of
      Nothing -> Client.sendTelegramMessage manager cfg.botToken cfg.chatId message
      Just imageUrl -> Client.sendTelegramPhoto manager cfg.botToken cfg.chatId message imageUrl
    case result of
      Left err -> throwError $ NotificationError err
      Right () -> pass
