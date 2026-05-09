module Moe.Libs.Sqlite.Migration
  ( Migration (..),
    MigrationRecord (..),
    MigrationResult (..),
    MigrationError (..),
    runMigrationsFromList,
    parseMigrationFilename,
  )
where

import Control.Exception (try)
import Data.Set qualified as Set
import Data.Text (pack)
import Data.Text.Encoding qualified as TE
import Data.Time (UTCTime)
import Database.SQLite.Simple (Connection, FromRow (..), Only (..), SQLError, execute_, field, query_)
import Database.SQLite.Simple qualified as SQL
import Database.SQLite3 qualified as SQLite3
import Relude
import System.FilePath (takeBaseName)

data Migration = Migration
  { version :: Text,
    name :: Text,
    sql :: ByteString
  }
  deriving stock (Show, Eq)

data MigrationRecord = MigrationRecord
  { version :: Text,
    name :: Text,
    executedAt :: UTCTime
  }
  deriving stock (Show, Eq)

instance FromRow MigrationRecord where
  fromRow = MigrationRecord <$> field <*> field <*> field

data MigrationResult
  = MigrationSuccess Int
  | MigrationNoOp
  deriving stock (Show, Eq)

data MigrationError
  = InvalidFilename FilePath Text
  | ReadError FilePath Text
  | SqlError Text Text
  | DirectoryNotFound FilePath
  deriving stock (Show, Eq)

runMigrationsFromList :: Connection -> [Migration] -> IO (Either MigrationError MigrationResult)
runMigrationsFromList conn migrations = do
  createMigrationsTable conn
  executed <- getExecutedVersions conn
  let pending = filter (\m -> m.version `Set.notMember` executed) migrations
      sorted = sortOn (.version) pending
  if null sorted
    then pure $ Right MigrationNoOp
    else runPendingMigrations conn sorted

createMigrationsTable :: Connection -> IO ()
createMigrationsTable conn =
  execute_
    conn
    "CREATE TABLE IF NOT EXISTS _migrations (\
    \  version TEXT PRIMARY KEY NOT NULL,\
    \  name TEXT NOT NULL,\
    \  executed_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL\
    \)"

parseMigrationFilename :: FilePath -> Either Text (Text, Text)
parseMigrationFilename filename =
  let baseName = takeBaseName filename
      (versionPart, rest) = break (== '_') baseName
   in case () of
        _
          | length versionPart /= 14 ->
              Left $ "version must be exactly 14 digits, got " <> pack (show (length versionPart))
          | not (all (`elem` ['0' .. '9']) versionPart) ->
              Left "version must contain only digits (0-9)"
          | null rest || rest == "_" ->
              Left "missing name part after version (expected {version}_{name}.sql)"
          | otherwise ->
              case rest of
                ('_' : namePart)
                  | not (null namePart) -> Right (pack versionPart, pack namePart)
                  | otherwise -> Left "name cannot be empty"
                _ -> Left "version and name must be separated by underscore"

getExecutedVersions :: Connection -> IO (Set.Set Text)
getExecutedVersions conn = do
  rows <- query_ conn "SELECT version FROM _migrations" :: IO [Only Text]
  pure $ Set.fromList $ map fromOnly rows

runPendingMigrations :: Connection -> [Migration] -> IO (Either MigrationError MigrationResult)
runPendingMigrations conn migrations = do
  result <- try $ SQL.withTransaction conn $ do
    mapM_ executeMigration migrations
  case result of
    Left (err :: SQLError) ->
      pure $ Left $ SqlError "Migration failed" (pack $ show err)
    Right _ ->
      pure $ Right $ MigrationSuccess (length migrations)
 where
  executeMigration :: Migration -> IO ()
  executeMigration migration = do
    let sqlText = TE.decodeUtf8Lenient migration.sql
    SQLite3.exec (SQL.connectionHandle conn) sqlText
    SQL.execute
      conn
      "INSERT INTO _migrations (version, name) VALUES (?, ?)"
      (migration.version, migration.name)
