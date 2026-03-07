-- | Structured error types for notification services.
module Moe.Infra.Notification.Types
  ( NotificationError (..),
  )
where

import Data.Text.Display (Display (..))
import Moe.Prelude
import Servant.Client (ClientError)

-- | Structured notification send errors.
data NotificationError
  = NetworkError ClientError
  | ConfigError Text

instance Display NotificationError where
  displayBuilder = \case
    NetworkError err -> "Notification network error: " <> displayBuilder (show @Text err)
    ConfigError msg -> "Notification: " <> displayBuilder msg
