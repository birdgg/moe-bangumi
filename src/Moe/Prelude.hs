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
    (:>),
    runEff,
    DispatchOf,
    Dispatch (..),
    withUnliftStrategy,
    UnliftStrategy (..),
    Persistence (..),
    Limit (..),
    withRunInIO,
    -- Effectful.Error.Static
    Error,
    throwError,
    catchError,
    runErrorWith,
    -- Effectful.Reader.Static
    Reader,
    ask,
    runReader,
    -- Effectful.Log
    Log,
    Logger,
    LogLevel (..),
    -- Effectful.Concurrent
    Concurrent,
    runConcurrent,
    threadDelay,
    -- Effectful.Concurrent.Async
    forConcurrently,
    withAsync,
    -- Effectful.FileSystem
    FileSystem,
    runFileSystem,
    doesDirectoryExist,
    doesFileExist,
    listDirectory,
    removeFile,
    renameFile,
    createDirectoryIfMissing,
    removePathForcibly,
    copyFile,
    copyFileWithMetadata,
    getPermissions,
    setPermissions,
    -- Effectful.Sqlite
    Sqlite,
    SqliteTransaction,
    transact,
    notransact,
    FromField (..),
    ToField (..),
    FromRow (..),
    ToRow (..),
    field,
    Only (..),
    type (:.)(..),
    execute,
    query,
    query_,
    SqliteDb (..),
    SqlitePool (..),
    runSqlite,
    -- Effectful.Exception
    try,
    throwIO,
    bracket_,
    -- Effectful.Dispatch.Dynamic
    interpret,
    -- Validation
    Validation,
    failureToMaybe,
    failureUnless,
  )
where

import Data.Text qualified as T
import Effectful (Dispatch (..), DispatchOf, Eff, Effect, IOE, Limit (..), Persistence (..), UnliftStrategy (..), runEff, withRunInIO, withUnliftStrategy, (:>))
import Effectful.Concurrent (Concurrent, runConcurrent, threadDelay)
import Effectful.Concurrent.Async (forConcurrently, withAsync)
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Error.Static (Error, catchError, runErrorWith, throwError)
import Effectful.Exception (bracket_, throwIO, try)
import Effectful.FileSystem (FileSystem, copyFile, copyFileWithMetadata, createDirectoryIfMissing, doesDirectoryExist, doesFileExist, getPermissions, listDirectory, removeFile, removePathForcibly, renameFile, runFileSystem, setPermissions)
import Effectful.Log (Log, LogLevel (..), Logger)
import Effectful.Reader.Static (Reader, ask, runReader)
import Effectful.Sqlite (FromField (..), FromRow (..), Only (..), Sqlite, SqliteDb (..), SqlitePool (..), SqliteTransaction, ToField (..), ToRow (..), execute, field, notransact, query, query_, runSqlite, transact, type (:.)(..))
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
