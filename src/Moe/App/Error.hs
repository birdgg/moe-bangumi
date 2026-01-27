module Moe.App.Error
  ( AppError (..),
  )
where

data AppError
  = DatabaseError Text
  | NotFound Text
  | ValidationError Text
  | ExternalApiError Text
  deriving stock (Eq, Show)
