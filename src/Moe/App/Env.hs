module Moe.App.Env
  ( MoeConfig (..),
    MoeEnv (..),
    AppEnv (..),
    defaultMoeConfig,
    parseMoeConfig,
    mkMoeEnv,
    mkDbPool,
    destroyDbPool,
    getDatabasePath,
    getSettingPath,
    getLogPath,
  )
where

import Data.Aeson (eitherDecodeFileStrict)
import Data.Pool qualified as Pool
import Data.Text.Display (Display (..))
import Database.SQLite.Simple qualified as Sqlite
import Effectful
import Effectful.Sqlite (SqlitePool (..))
import GHC.Conc (getNumCapabilities)
import Moe.App.Logging (LogConfig (..), LogDestination (..), defaultLogConfig)
import Moe.Domain.Setting.Types (UserPreference, defaultUserPreference)
import Moe.Prelude
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.Directory (doesFileExist)
import System.FilePath ((</>))

data AppEnv
  = Development
  | Production
  deriving stock (Eq, Show, Enum, Bounded)

instance Display AppEnv where
  displayBuilder Development = "dev"
  displayBuilder Production = "prod"

data MoeConfig = MoeConfig
  { appEnv :: AppEnv,
    port :: Int,
    dataFolder :: FilePath,
    logConfig :: LogConfig
  }
  deriving stock (Eq, Show)

data MoeEnv = MoeEnv
  { config :: MoeConfig,
    settingVar :: TVar UserPreference,
    httpManager :: Manager,
    dbPool :: SqlitePool
  }

defaultMoeConfig :: MoeConfig
defaultMoeConfig =
  MoeConfig
    { appEnv = Development,
      port = 3000,
      dataFolder = "./data",
      logConfig = defaultLogConfig
    }

parseMoeConfig :: (IOE :> es) => Eff es MoeConfig
parseMoeConfig = do
  appEnv <- parseAppEnv <$> liftIO (lookupEnv "MOE_ENV")
  port <- parsePort <$> liftIO (lookupEnv "MOE_PORT")
  dataFolder <- parseDataFolder <$> liftIO (lookupEnv "MOE_DATA_FOLDER")
  logDest <- parseLogDest dataFolder <$> liftIO (lookupEnv "MOE_LOG_DEST")
  let logConfig = LogConfig {destination = logDest, logLevel = defaultLogConfig.logLevel}
  pure MoeConfig {appEnv, port, dataFolder, logConfig}
  where
    parseAppEnv :: Maybe String -> AppEnv
    parseAppEnv (Just "prod") = Production
    parseAppEnv (Just "production") = Production
    parseAppEnv _ = Development

    parsePort :: Maybe String -> Int
    parsePort mstr = fromMaybe 3000 (mstr >>= readMaybe)

    parseDataFolder :: Maybe String -> FilePath
    parseDataFolder = fromMaybe "./data"

    parseLogDest :: FilePath -> Maybe String -> LogDestination
    parseLogDest folder (Just "file") = JsonFile (folder </> "moe-bangumi.log")
    parseLogDest _ _ = PrettyStdOut

mkMoeEnv :: (IOE :> es) => MoeConfig -> Eff es MoeEnv
mkMoeEnv config = do
  let settingPath = config.dataFolder </> "setting.json"
  initialSetting <- loadSettingFromFile settingPath
  settingVar <- liftIO $ newTVarIO initialSetting
  httpManager <- liftIO $ newManager tlsManagerSettings
  dbPool <- mkDbPool (getDatabasePath' config)
  pure MoeEnv {config, settingVar, httpManager, dbPool}

mkDbPool :: (IOE :> es) => FilePath -> Eff es SqlitePool
mkDbPool dbPath = liftIO $ do
  numCapabilities <- getNumCapabilities
  fmap SqlitePool $
    Pool.newPool $
      Pool.defaultPoolConfig
        (openDb dbPath)
        Sqlite.close
        0.5
        numCapabilities

openDb :: FilePath -> IO Sqlite.Connection
openDb dbPath = do
  conn <- Sqlite.open dbPath
  Sqlite.execute_ conn "PRAGMA busy_timeout = 3000"
  pure conn

destroyDbPool :: MoeEnv -> IO ()
destroyDbPool env = Pool.destroyAllResources (getPool env.dbPool)

loadSettingFromFile :: (IOE :> es) => FilePath -> Eff es UserPreference
loadSettingFromFile path = do
  exists <- liftIO $ doesFileExist path
  if exists
    then fromRight defaultUserPreference <$> liftIO (eitherDecodeFileStrict path)
    else pure defaultUserPreference

getDatabasePath :: MoeEnv -> FilePath
getDatabasePath env = getDatabasePath' env.config

getDatabasePath' :: MoeConfig -> FilePath
getDatabasePath' cfg = cfg.dataFolder </> "moe-bangumi.db"

getSettingPath :: MoeEnv -> FilePath
getSettingPath env = env.config.dataFolder </> "setting.json"

getLogPath :: MoeEnv -> FilePath
getLogPath env = env.config.dataFolder </> "moe-bangumi.log"
