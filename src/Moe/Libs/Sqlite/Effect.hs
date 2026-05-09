module Moe.Libs.Sqlite.Effect
  ( Sqlite,
    SqliteTransaction,
    runSqlite,
    runSqliteDebug,
    SqlitePool (..),
    withSqlPool,
    transact,
    transactImmediate,
    transactExclusive,
    notransact,
    query,
    query_,
    execute,
    execute_,
    executeMany,
    queryNamed,
    executeNamed,
    executeNamedReturningChanges,
    savepoint,
    runMigrationsFromList,
    parseMigrationFilename,
    retryBusy,
  )
where

import Control.Concurrent (getNumCapabilities)
import Control.Exception (mask, onException)
import Data.Pool qualified as Pool
import Data.Text qualified as T
import Database.SQLite.Simple (Connection, Error (..), FromRow, NamedParam, Query, SQLError (..), ToRow)
import Database.SQLite.Simple qualified as SQL
import Effectful
import Effectful.Concurrent (Concurrent, threadDelay)
import Effectful.Dispatch.Static
import Effectful.Exception qualified as E
import Effectful.Log (Log, logAttention_)
import Moe.Libs.Sqlite.Migration qualified as M
import Moe.Libs.Sqlite.Migration (Migration (..), MigrationError, MigrationResult)
import Moe.Libs.Sqlite.Types (SqliteDb (..), SqlitePool (..))
import Relude
import System.Random (randomRIO)
import Text.Printf (printf)

type SqliteLogger = String -> IO ()

noLogger :: SqliteLogger
noLogger = const (pure ())

data Sqlite :: Effect

type instance DispatchOf Sqlite = 'Static 'WithSideEffects

data instance StaticRep Sqlite = SqliteRep SqliteLogger SqlitePool

data SqliteTransaction :: Effect

type instance DispatchOf SqliteTransaction = 'Static 'NoSideEffects

data instance StaticRep SqliteTransaction = SqliteTxRep SqliteLogger Connection

query ::
  (SqliteTransaction :> es, IOE :> es, ToRow q, FromRow r) =>
  Query ->
  q ->
  Eff es [r]
query q params = do
  SqliteTxRep logger conn <- getStaticRep
  liftIO $ do
    logger (show q)
    SQL.query conn q params

query_ ::
  (SqliteTransaction :> es, IOE :> es, FromRow r) =>
  Query ->
  Eff es [r]
query_ q = do
  SqliteTxRep logger conn <- getStaticRep
  liftIO $ do
    logger (show q)
    SQL.query_ conn q

queryNamed ::
  (SqliteTransaction :> es, IOE :> es, FromRow r) =>
  Query ->
  [NamedParam] ->
  Eff es [r]
queryNamed q params = do
  SqliteTxRep logger conn <- getStaticRep
  liftIO $ do
    logger (show q)
    SQL.queryNamed conn q params

execute ::
  (SqliteTransaction :> es, IOE :> es, ToRow q) =>
  Query ->
  q ->
  Eff es ()
execute q params = do
  SqliteTxRep logger conn <- getStaticRep
  liftIO $ do
    logger (show q)
    SQL.execute conn q params

execute_ ::
  (SqliteTransaction :> es, IOE :> es) =>
  Query ->
  Eff es ()
execute_ q = do
  SqliteTxRep logger conn <- getStaticRep
  liftIO $ do
    logger (show q)
    SQL.execute_ conn q

executeMany ::
  (SqliteTransaction :> es, IOE :> es, ToRow q) =>
  Query ->
  [q] ->
  Eff es ()
executeMany q params = do
  SqliteTxRep logger conn <- getStaticRep
  liftIO $ do
    logger (show q)
    SQL.executeMany conn q params

executeNamed ::
  (SqliteTransaction :> es, IOE :> es) =>
  Query ->
  [NamedParam] ->
  Eff es ()
executeNamed q params = do
  SqliteTxRep logger conn <- getStaticRep
  liftIO $ do
    logger (show q)
    SQL.executeNamed conn q params

executeNamedReturningChanges ::
  (SqliteTransaction :> es, IOE :> es) =>
  Query ->
  [NamedParam] ->
  Eff es Int
executeNamedReturningChanges q params = do
  SqliteTxRep logger conn <- getStaticRep
  liftIO $ do
    logger (show q)
    SQL.executeNamed conn q params
    SQL.changes conn

withResourceEff :: Pool.Pool a -> (a -> Eff es r) -> Eff es r
withResourceEff pool act = unsafeEff $ \es ->
  mask $ \unmask -> do
    (resource, localPool) <- Pool.takeResource pool
    r <-
      unmask (unEff (act resource) es)
        `onException` Pool.destroyResource pool localPool resource
    Pool.putResource localPool resource
    pure r

retryBusy ::
  (Concurrent :> es, Log :> es, IOE :> es) =>
  Eff es r ->
  Eff es r
retryBusy act =
  (10 :: Int, 0.5e6 :: Double)
    & fix
      ( \self (!left, !wait) -> do
          when (left <= 0) $
            E.throwIO $
              SQLError
                { sqlErrorDetails = "Busy retrial limit exceeded",
                  sqlErrorContext = "retryBusy",
                  sqlError = ErrorBusy
                }
          act `E.catch` \exc@SQLError {sqlError} ->
            case sqlError of
              ErrorBusy -> do
                wait' <- liftIO $ randomRIO (1e-6, wait)
                logAttention_ $
                  "ErrorBusy detected. Retrying after "
                    <> T.pack (printf "%.06f" (wait' * 1e-6))
                    <> " seconds..."
                threadDelay $ floor wait'
                self (left - 1, wait * 1.5)
              _ -> E.throwIO exc
      )

notransact ::
  (Sqlite :> es, Concurrent :> es, Log :> es, IOE :> es) =>
  Eff (SqliteTransaction : es) a ->
  Eff es a
notransact action = do
  SqliteRep logger (SqlitePool pool) <- getStaticRep
  withResourceEff pool $ \conn ->
    evalStaticRep (SqliteTxRep logger conn) $
      retryBusy action

transact ::
  (Sqlite :> es, Concurrent :> es, Log :> es, IOE :> es) =>
  Eff (SqliteTransaction : es) a ->
  Eff es a
transact = transactWith SQL.withTransaction

transactImmediate ::
  (Sqlite :> es, Concurrent :> es, Log :> es, IOE :> es) =>
  Eff (SqliteTransaction : es) a ->
  Eff es a
transactImmediate = transactWith SQL.withImmediateTransaction

transactExclusive ::
  (Sqlite :> es, Concurrent :> es, Log :> es, IOE :> es) =>
  Eff (SqliteTransaction : es) a ->
  Eff es a
transactExclusive = transactWith SQL.withExclusiveTransaction

transactWith ::
  (Sqlite :> es, Concurrent :> es, Log :> es, IOE :> es) =>
  (Connection -> IO a -> IO a) ->
  Eff (SqliteTransaction : es) a ->
  Eff es a
transactWith withTx action = do
  SqliteRep logger (SqlitePool pool) <- getStaticRep
  withResourceEff pool $ \conn ->
    unsafeEff $ \es ->
      withTx conn $
        unEff (evalStaticRep (SqliteTxRep logger conn) (retryBusy action)) es

savepoint ::
  (SqliteTransaction :> es, IOE :> es) =>
  Eff es a ->
  Eff es a
savepoint action = do
  SqliteTxRep _logger conn <- getStaticRep
  unsafeEff $ \es -> SQL.withSavepoint conn $ unEff action es

withSqlPool ::
  (IOE :> es) =>
  FilePath ->
  (SqlitePool -> Eff es a) ->
  Eff es a
withSqlPool dbPath =
  E.bracket
    ( liftIO $ do
        num <- getNumCapabilities
        fmap SqlitePool $
          Pool.newPool $
            Pool.defaultPoolConfig
              ( do
                  conn <- SQL.open dbPath
                  SQL.execute_ conn "PRAGMA busy_timeout=3000;"
                  pure conn
              )
              SQL.close
              0.5
              num
    )
    (liftIO . Pool.destroyAllResources . getPool)

runMigrationsFromList ::
  (Sqlite :> es, IOE :> es) =>
  [Migration] ->
  Eff es (Either MigrationError MigrationResult)
runMigrationsFromList migrations = do
  SqliteRep _logger (SqlitePool pool) <- getStaticRep
  withResourceEff pool $ \conn ->
    liftIO $ M.runMigrationsFromList conn migrations

parseMigrationFilename :: FilePath -> Either Text (Text, Text)
parseMigrationFilename = M.parseMigrationFilename

runSqlite ::
  (IOE :> es) =>
  SqliteDb ->
  Eff (Sqlite : es) a ->
  Eff es a
runSqlite (DbFile dbPath) act =
  withSqlPool dbPath $ \pool ->
    evalStaticRep (SqliteRep noLogger pool) act
runSqlite (DbPool pool) act =
  evalStaticRep (SqliteRep noLogger pool) act
{-# INLINE runSqlite #-}

runSqliteDebug ::
  (IOE :> es) =>
  (String -> IO ()) ->
  SqliteDb ->
  Eff (Sqlite : es) a ->
  Eff es a
runSqliteDebug logger (DbFile dbPath) act =
  withSqlPool dbPath $ \pool ->
    evalStaticRep (SqliteRep logger pool) act
runSqliteDebug logger (DbPool pool) act =
  evalStaticRep (SqliteRep logger pool) act
{-# INLINE runSqliteDebug #-}
