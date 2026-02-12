{-# LANGUAGE DerivingVia #-}

-- | Shared numbering types for media organization.
module Moe.Domain.Shared.Numbering
  ( SeasonNumber (..),
    EpisodeNumber (..),
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

-- | Season number, zero-padded for display.
newtype SeasonNumber = SeasonNumber {getSeason :: Word8}
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Num, ToJSON, FromJSON, ToSchema, FromField, ToField)
  deriving (ToText) via (Padded Word8)

-- | Episode number, zero-padded for display.
newtype EpisodeNumber = EpisodeNumber {getEpisode :: Word32}
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Num, ToJSON, FromJSON, ToSchema, FromField, ToField)
  deriving (ToText) via (Padded Word32)

-- | Subtract an offset from an episode number.
-- Keeps the original number if it is not greater than the offset.
applyEpisodeOffset :: Word32 -> EpisodeNumber -> EpisodeNumber
applyEpisodeOffset 0 n = n
applyEpisodeOffset offset n
  | n.getEpisode > offset = EpisodeNumber (n.getEpisode - offset)
  | otherwise = n
