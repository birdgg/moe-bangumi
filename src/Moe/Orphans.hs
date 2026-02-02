{-# OPTIONS_GHC -Wno-orphans #-}

module Moe.Orphans () where

import Data.Text qualified as T
import Data.Time.Calendar (Year)
import Relude (ToText (..))

instance ToText Year where
  toText = T.pack . show
