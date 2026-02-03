module Moe.Infrastructure.Download.Effect
  ( Download (..),
    DownloadError (..),
    addTorrent,
  )
where

import Data.Text (Text)
import Data.Text.Display (Display (..))
import Effectful
import Effectful.Dispatch.Dynamic (send)
import Effectful.Error.Static (Error)

data DownloadError
  = LoginFailed Text
  | AddTorrentFailed Text
  | ConfigMissing
  deriving stock (Eq, Show)

instance Display DownloadError where
  displayBuilder (LoginFailed msg) = "Login failed: " <> displayBuilder msg
  displayBuilder (AddTorrentFailed msg) = "Add torrent failed: " <> displayBuilder msg
  displayBuilder ConfigMissing = "Downloader config missing"

data Download :: Effect where
  AddTorrent :: Error DownloadError :> es => Text -> Maybe Text -> Download (Eff es) ()

type instance DispatchOf Download = Dynamic

addTorrent :: (Download :> es, Error DownloadError :> es) => Text -> Maybe Text -> Eff es ()
addTorrent url savePath = send (AddTorrent url savePath)
