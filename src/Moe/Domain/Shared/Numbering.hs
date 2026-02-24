{-# LANGUAGE DerivingVia #-}

-- | Shared numbering types for media organization.
module Moe.Domain.Shared.Numbering
  ( SeasonIndex (..),
    EpisodeIndex (..),
    applyEpisodeOffset,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToSchema)
import Effectful.Sqlite (FromField, ToField)
import Moe.Prelude

-- | Zero-pad a number to at least two digits.
paddingZero :: (Integral a, Show a) => a -> Text
paddingZero n
  | n < 10 = "0" <> show n
  | otherwise = show n

newtype Padded a = Padded a

instance (Integral a, Show a) => ToText (Padded a) where
  toText (Padded n) = paddingZero n

-- | Season index, zero-padded for display.
newtype SeasonIndex = SeasonIndex {getSeason :: Word8}
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Num, ToJSON, FromJSON, ToSchema, FromField, ToField)
  deriving (ToText) via (Padded Word8)

-- | Episode index, zero-padded for display.
newtype EpisodeIndex = EpisodeIndex {getEpisode :: Word32}
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Num, ToJSON, FromJSON, ToSchema, FromField, ToField)
  deriving (ToText) via (Padded Word32)

-- | Subtract an offset from an episode number.
-- Keeps the original number if it is not greater than the offset.
applyEpisodeOffset :: Word32 -> EpisodeIndex -> EpisodeIndex
applyEpisodeOffset 0 n = n
applyEpisodeOffset offset n
  | n.getEpisode > offset = EpisodeIndex (n.getEpisode - offset)
  | otherwise = n
