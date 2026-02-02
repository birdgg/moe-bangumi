{-# OPTIONS_GHC -Wno-orphans #-}

module Moe.Orphans () where

import Data.Text qualified as T
import Data.Text.Conversions (ToText (..))
import Data.Time.Calendar (Year)

instance ToText Year where
  toText = T.pack . show
