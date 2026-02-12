{-# LANGUAGE NoImplicitPrelude #-}

module Moe.Prelude
  ( liftEither,
    liftEitherWith,
    validateField,
    fmtErrors,
    FromText (..),
    module Relude,
    -- Effectful core
    Eff,
    IOE,
    Effect,
    runEff,
    Error,
    throwError,
    -- Effectful effects
    Concurrent,
    FileSystem,
    Log,
    Reader,
    Sqlite,
    Validation,
    failureToMaybe,
    failureUnless,
  )
where

import Data.Text qualified as T
import Effectful (Eff, Effect, IOE, runEff, (:>))
import Effectful.Concurrent (Concurrent)
import Effectful.Error.Static (Error, throwError)
import Effectful.FileSystem (FileSystem)
import Effectful.Log (Log)
import Effectful.Reader.Static (Reader)
import Effectful.Sqlite (Sqlite)
import Relude hiding (MonadReader, Reader, ReaderT, ask, asks, local, runReader, runReaderT)
import Validation (Validation (..), failureToMaybe, failureUnless)

liftEither :: (Show e, Error e :> es) => Either e a -> Eff es a
liftEither = either throwError pure

-- | Lift an Either into an effect, converting the error type.
liftEitherWith :: (Show e', Error e' :> es) => (e -> e') -> Either e a -> Eff es a
liftEitherWith f = either (throwError . f) pure

-- | Validate that a text field is non-empty.
validateField :: Text -> Text -> Validation (NonEmpty Text) ()
validateField name value = failureUnless (not $ T.null value) name

-- | Format validation errors with a prefix, returning Nothing on success.
fmtErrors :: Text -> Validation (NonEmpty Text) a -> Maybe Text
fmtErrors prefix v =
  failureToMaybe v <&> \errs ->
    prefix <> ": " <> T.intercalate ", " (toList errs) <> " must not be empty"

class (Bounded a, Enum a, ToText a) => FromText a where
  fromText :: Text -> Maybe a
  fromText = inverseMap toText
