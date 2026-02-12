-- | Abstract Notification effect for sending alerts.
--
-- Provides a transport-agnostic interface that can be implemented
-- by Telegram, Discord, or other notification services.
module Moe.Infra.Notification.Effect
  ( -- * Effect
    Notification (..),

    -- * Operations
    sendNotification,
  )
where

import Effectful
import Effectful.TH (makeEffect)
import Moe.Prelude

-- | Abstract notification effect.
data Notification :: Effect where
  -- | Send a notification with message text and optional image URL.
  SendNotification :: Text -> Maybe Text -> Notification m ()

makeEffect ''Notification
