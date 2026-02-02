module Moe.Error
  ( MoeError (..),
    liftError,
  )
where

import Data.Bifunctor (first)
import Data.Text (Text)
import Data.Text qualified as T
import Effectful (Eff, (:>))
import Effectful.Error.Static (Error)
import Moe.Prelude (liftEither)

data MoeError
  = DatabaseError Text
  | NotFound Text
  | ValidationError Text
  | ExternalApiError Text
  deriving stock (Eq, Show)

liftError :: (Show e, Error MoeError :> es) => (Text -> MoeError) -> Text -> Either e a -> Eff es a
liftError mkErr msg = liftEither . first (mkErr . (msg <>) . T.pack . show)
