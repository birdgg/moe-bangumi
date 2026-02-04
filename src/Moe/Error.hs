module Moe.Error
  ( MoeError (..),
    liftError,
  )
where

import Effectful (Eff, (:>))
import Effectful.Error.Static (Error)
import Moe.Prelude

data MoeError
  = DatabaseError Text
  | NotFound Text
  | ValidationError Text
  | ExternalApiError Text
  deriving stock (Eq, Show)

liftError :: (Show e, Error MoeError :> es) => (Text -> MoeError) -> Text -> Either e a -> Eff es a
liftError mkErr msg = liftEither . first (mkErr . (msg <>) . show)
