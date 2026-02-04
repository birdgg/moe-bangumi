module Moe.Domain.Bangumi.Internal.Metadata
  ( TmdbId (..),
    BgmtvId (..),
    MikanId (..),
  )
where

import Moe.Prelude

newtype TmdbId = TmdbId Word32
  deriving stock (Eq, Show)
  deriving newtype (Num)

newtype BgmtvId = BgmtvId Word32
  deriving stock (Eq, Show)
  deriving newtype (Num)

newtype MikanId = MikanId Word32
  deriving stock (Eq, Show)
  deriving newtype (Num)
