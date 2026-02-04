{-# OPTIONS_GHC -Wno-orphans #-}

module Moe.Orphans () where

import Data.Time.Calendar (Year)
import Moe.Prelude

instance ToText Year where
  toText = show
