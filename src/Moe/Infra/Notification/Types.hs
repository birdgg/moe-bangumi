-- | Structured error types for notification services.
module Moe.Infra.Notification.Types
  ( NotificationSendError (..),
  )
where

import Data.Text.Display (Display (..))
import Moe.Prelude

-- | Structured notification send errors.
data NotificationSendError
  = NtfNetworkError Text
  | NtfHttpError Int
  | NtfConfigError Text
  deriving stock (Show, Eq)

instance Display NotificationSendError where
  displayBuilder = \case
    NtfNetworkError msg -> "Notification: network error: " <> displayBuilder msg
    NtfHttpError code -> "Notification: HTTP " <> displayBuilder (show @Text code)
    NtfConfigError msg -> "Notification: config error: " <> displayBuilder msg
