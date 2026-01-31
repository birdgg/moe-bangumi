module Moe.Prelude
  ( inverseMap,
  )
where

import Data.Map.Strict qualified as Map

inverseMap :: (Bounded a, Enum a, Ord k) => (a -> k) -> k -> Maybe a
inverseMap f = \k -> Map.lookup k mapping
  where
    mapping = Map.fromList [(f a, a) | a <- [minBound .. maxBound]]
