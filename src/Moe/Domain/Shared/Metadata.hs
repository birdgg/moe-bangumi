module Moe.Domain.Shared.Metadata
  ( TmdbId (..),
    BgmtvId (..),
    MikanId (..),
  )
where

import Effectful.Sqlite (FromField, ToField)
import Moe.Prelude

newtype TmdbId = TmdbId Word32
  deriving stock (Eq, Show)
  deriving newtype (Num, FromField, ToField)

newtype BgmtvId = BgmtvId Word32
  deriving stock (Eq, Show)
  deriving newtype (Num, FromField, ToField)

newtype MikanId = MikanId Word32
  deriving stock (Eq, Show)
  deriving newtype (Num, FromField, ToField)
