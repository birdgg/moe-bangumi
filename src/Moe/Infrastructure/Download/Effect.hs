module Moe.Infrastructure.Download.Effect
  ( Download (..),
    DownloadError (..),
    login,
    addTorrent,
  )
where

import Effectful
import Moe.Prelude (Maybe, Text)
import Effectful.TH (makeEffect)
import Moe.Infrastructure.Download.Types (DownloadError (..))

data Download :: Effect where
  Login :: Download m ()
  AddTorrent :: Text -> Maybe Text -> Download m ()

makeEffect ''Download
