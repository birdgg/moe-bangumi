module Moe.Prelude
  ( liftEither,
    module Relude,
  )
where

import Effectful (Eff, (:>))
import Effectful.Error.Static (Error, throwError)
import Relude

liftEither :: (Show e, Error e :> es) => Either e a -> Eff es a
liftEither = either throwError pure
