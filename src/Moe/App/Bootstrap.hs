module Moe.App.Bootstrap
  ( bootstrap,
  )
where

import Effectful
import Effectful.Log qualified as Log
import Effectful.Sqlite (SqliteDb (..), notransact, runMigrations, runSqlite)
import Moe.App.Env (MoeConfig (..), MoeEnv (..), mkMoeEnv, parseMoeConfig)
import Moe.App.Logging (LogConfig (..), makeLogger, runLog)
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

runDatabaseMigrations :: (IOE :> es, Concurrent :> es) => MoeEnv -> Eff es ()
runDatabaseMigrations env =
  makeLogger env.config.dataFolder $ \logger ->
    runLog "bootstrap" logger env.config.logConfig.logLevel $
      runSqlite (DbPool env.dbPool) $
        notransact $ do
          Log.logInfo_ "Running database migrations"
          runMigrations "migrations"
          Log.logInfo_ "Database migrations completed"
