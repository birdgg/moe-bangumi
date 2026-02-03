module Moe.Domain.Bangumi.Metadata.Types
  ( TmdbId (..),
    BgmtvId (..),
    MikanId (..),
  )
where

import Data.Word (Word32)

newtype TmdbId = TmdbId Word32
  deriving stock (Eq, Show)
  deriving newtype (Num)

newtype BgmtvId = BgmtvId Word32
  deriving stock (Eq, Show)
  deriving newtype (Num)

newtype MikanId = MikanId Word32
  deriving stock (Eq, Show)
  deriving newtype (Num)
