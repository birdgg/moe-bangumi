module Moe.Infrastructure.Download.Effect
  ( Download (..),
    DownloadError (..),
    addTorrent,
  )
where

import Data.Text (Text)
import Effectful
import Effectful.TH

data DownloadError
  = LoginFailed Text
  | AddTorrentFailed Text
  | ConfigMissing
  deriving stock (Eq, Show)

data Download :: Effect where
  AddTorrent :: Text -> Maybe Text -> Download m (Either DownloadError ())

type instance DispatchOf Download = Dynamic

makeEffect ''Download
