{-# LANGUAGE DerivingVia #-}

module Moe.Domain.Bangumi.Internal.Episode
  ( EpisodeNumber (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToSchema)
import Moe.Prelude

newtype EpisodeNumber = EpisodeNumber Word32
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Num, ToJSON, FromJSON, ToSchema)
  deriving (ToText) via (Padded Word32)
