{-# LANGUAGE DerivingVia #-}

module Moe.Domain.Bangumi.Internal.Episode
  ( EpisodeNumber (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToSchema)
import Data.Word (Word32)
import GHC.Generics (Generic)
import Moe.Prelude (Padded (..), ToText)

newtype EpisodeNumber = EpisodeNumber Word32
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Num, ToJSON, FromJSON, ToSchema)
  deriving (ToText) via (Padded Word32)
