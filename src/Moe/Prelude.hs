module Moe.Prelude
  ( inverseMap,
    liftEither,
  )
where

import Data.Map.Strict qualified as Map
import Effectful (Eff, (:>))
import Effectful.Error.Static (Error, throwError)

inverseMap :: (Bounded a, Enum a, Ord k) => (a -> k) -> k -> Maybe a
inverseMap f = (`Map.lookup` mapping)
  where
    mapping = Map.fromList [(f a, a) | a <- [minBound .. maxBound]]

liftEither :: (Show e, Error e :> es) => Either e a -> Eff es a
liftEither = either throwError pure
