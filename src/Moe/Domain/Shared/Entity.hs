module Moe.Domain.Shared.Entity
  ( Id (..),
    Entity (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text.Display (Display (..))
import Data.Time (UTCTime)
import Effectful.Sqlite (FromField (..), FromRow (..), ToField (..), field)
import Moe.Prelude

-- | Phantom-typed database primary key.
-- Id Bangumi /= Id Episode at compile time.
newtype Id a = Id {getId :: Int64}
  deriving stock (Eq, Show, Ord)
  deriving newtype (FromJSON, ToJSON, FromField, ToField)

instance Display (Id a) where
  displayBuilder (Id i) = displayBuilder i

-- | Entity wraps a domain value with its database identity.
data Entity a = Entity
  { entityId :: Id a,
    entityVal :: a,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving stock (Eq, Show)

-- | Generic FromRow for Entity: reads id, then inner value, then created_at, updated_at.
-- Requires DB column order: id, ...fields..., created_at, updated_at.
instance (FromRow a) => FromRow (Entity a) where
  fromRow = Entity <$> field <*> fromRow <*> field <*> field
