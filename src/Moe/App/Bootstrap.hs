module Moe.App.Bootstrap
  ( bootstrap,
  )
where

import Effectful
import Effectful.Log qualified as Log
import Effectful.Sqlite (Migration (..), SqliteDb (..), notransact, parseMigrationFilename, runMigrationsFromList, runSqlite)
import Moe.App.Env (MoeConfig (..), MoeEnv (..), mkMoeEnv, parseMoeConfig)
import Moe.App.Logging (LogConfig (..), makeLogger, runLog)
import Moe.Infra.Database.Embedded (embeddedMigrations)
import Moe.Prelude
import System.Directory (createDirectoryIfMissing)

bootstrap :: (IOE :> es, Concurrent :> es, FileSystem :> es) => Eff es MoeEnv
bootstrap = do
  config <- parseMoeConfig
  ensureDataFolder config.dataFolder
  env <- mkMoeEnv config
  runDatabaseMigrations env
  pure env

ensureDataFolder :: (IOE :> es) => FilePath -> Eff es ()
ensureDataFolder = liftIO . createDirectoryIfMissing True

-- | Parse embedded migration files into Migration values.
parseEmbeddedMigrations :: [(FilePath, ByteString)] -> [Migration]
parseEmbeddedMigrations = mapMaybe parse
 where
  parse (filename, content) =
    case parseMigrationFilename filename of
      Left _ -> Nothing
      Right (ver, name) -> Just $ Migration ver name content

runDatabaseMigrations :: (IOE :> es, Concurrent :> es) => MoeEnv -> Eff es ()
runDatabaseMigrations env =
  makeLogger env.config.dataFolder $ \logger ->
    runLog "bootstrap" logger env.config.logConfig.logLevel $
      runSqlite (DbPool env.dbPool) $
        notransact $ do
          Log.logInfo_ "Running database migrations"
          let migrations = parseEmbeddedMigrations embeddedMigrations
          runMigrationsFromList migrations
          Log.logInfo_ "Database migrations completed"
