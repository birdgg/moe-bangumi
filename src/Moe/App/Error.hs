module Moe.App.Error
  ( MoeError (..),
  )
where

import Data.Text (Text)

data MoeError
  = DatabaseError Text
  | NotFound Text
  | ValidationError Text
  | ExternalApiError Text
  deriving stock (Eq, Show)
