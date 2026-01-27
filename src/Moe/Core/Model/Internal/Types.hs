module Moe.Core.Model.Internal.Types
  ( TmdbId (..),
    BgmtvId (..),
  )
where

newtype TmdbId = TmdbId Word32
  deriving stock (Eq, Show)
  deriving newtype (Num)

newtype BgmtvId = BgmtvId Word32
  deriving stock (Eq, Show)
  deriving newtype (Num)
