module Moe.Prelude
  ( liftEither,
    paddingZero,
    FromText (..),
    module Relude,
  )
where


import Effectful (Eff, (:>))
import Effectful.Error.Static (Error, throwError)
import Relude hiding (show)
import Relude qualified as R

liftEither :: (Show e, Error e :> es) => Either e a -> Eff es a
liftEither = either throwError pure

paddingZero :: (Integral a, Show a) => a -> Text
paddingZero n
  | n < 10 = "0" <> R.show n
  | otherwise = R.show n

class (Bounded a, Enum a, ToText a) => FromText a where
  fromText :: Text -> Maybe a
  fromText = inverseMap toText
