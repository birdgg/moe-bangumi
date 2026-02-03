{-# LANGUAGE DerivingVia #-}

module Moe.Domain.Bangumi.Internal.Season
  ( SeasonNumber (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToSchema)
import Data.Word (Word8)
import GHC.Generics (Generic)
import Moe.Prelude (Padded (..), ToText)

newtype SeasonNumber = SeasonNumber Word8
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Num, ToJSON, FromJSON, ToSchema)
  deriving (ToText) via (Padded Word8)
