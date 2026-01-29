module Moe.App.Error
  ( MoeError (..),
  )
where

data MoeError
  = DatabaseError Text
  | NotFound Text
  | ValidationError Text
  | ExternalApiError Text
  deriving stock (Eq, Show)
