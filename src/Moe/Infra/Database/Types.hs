-- | Structured error types for database operations.
module Moe.Infra.Database.Types
  ( DatabaseExecError (..),
  )
where

import Data.Text.Display (Display (..))
import Moe.Prelude

-- | Structured database execution errors.
data DatabaseExecError
  = DbUnexpectedResult Text
  | DbQueryError Text
  deriving stock (Show, Eq)

instance Display DatabaseExecError where
  displayBuilder = \case
    DbUnexpectedResult msg -> "Database unexpected result: " <> displayBuilder msg
    DbQueryError msg -> "Database query error: " <> displayBuilder msg
