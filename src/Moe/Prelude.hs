{-# LANGUAGE NoImplicitPrelude #-}

module Moe.Prelude
  ( liftEither,
    paddingZero,
    Padded (..),
    FromText (..),
    module Relude,
  )
where

import Effectful (Eff, (:>))
import Effectful.Error.Static (Error, throwError)
import Relude hiding (Reader)

liftEither :: (Show e, Error e :> es) => Either e a -> Eff es a
liftEither = either throwError pure

paddingZero :: (Integral a, Show a) => a -> Text
paddingZero n
  | n < 10 = "0" <> show n
  | otherwise = show n

newtype Padded a = Padded a

instance (Integral a, Show a) => ToText (Padded a) where
  toText (Padded n) = paddingZero n

class (Bounded a, Enum a, ToText a) => FromText a where
  fromText :: Text -> Maybe a
  fromText = inverseMap toText
