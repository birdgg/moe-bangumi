module Moe.App.Bootstrap
  ( bootstrap,
  )
where

import Effectful
import Effectful.Concurrent (Concurrent)
import Effectful.Log qualified as Log
import Effectful.Sqlite (SqliteDb (..), notransact, runMigrations, runSqlite)
import Moe.App.Env (MoeEnv (..), MoeConfig (..), getDatabasePath, mkMoeEnv, parseMoeConfig)
import Moe.App.Logging (LogConfig (..), makeLogger, runLog)
import System.Directory (createDirectoryIfMissing)

bootstrap :: (IOE :> es, Concurrent :> es) => Eff es MoeEnv
bootstrap = do
  env <- parseMoeConfig >>= mkMoeEnv
  ensureDataFolder env.config.dataFolder
  runDatabaseMigrations env
  pure env

ensureDataFolder :: (IOE :> es) => FilePath -> Eff es ()
ensureDataFolder = liftIO . createDirectoryIfMissing True

runDatabaseMigrations :: (IOE :> es, Concurrent :> es) => MoeEnv -> Eff es ()
runDatabaseMigrations env =
  makeLogger env.config.logConfig.destination $ \logger ->
    runLog "bootstrap" logger env.config.logConfig.logLevel $
      runSqlite (DbFile $ getDatabasePath env) $
        notransact $ do
          Log.logInfo_ "Running database migrations"
          runMigrations "migrations"
          Log.logInfo_ "Database migrations completed"
