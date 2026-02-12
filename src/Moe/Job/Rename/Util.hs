-- | Shared utilities for rename job modules.
module Moe.Job.Rename.Util
  ( notifySafe,
  )
where

import Effectful
import Effectful.Exception (try)
import Effectful.Log qualified as Log
import Moe.Infra.Notification.Effect (Notification, sendNotification)
import Moe.Prelude

-- | Send notification with bangumi title and optional poster image.
notifySafe ::
  (Notification :> es, Log :> es, IOE :> es) =>
  Text ->
  Maybe Text ->
  Eff es ()
notifySafe title mPosterUrl = do
  result <- try @SomeException $ sendNotification (title <> " 已更新") mPosterUrl
  case result of
    Left ex ->
      Log.logAttention_ $ "rename: notification failed - " <> toText (displayException ex)
    Right () -> pass
