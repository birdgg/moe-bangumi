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
  )
where

import Data.Pool qualified as Pool
import Data.Text.Display (Display (..))
import Database.SQLite.Simple qualified as Sqlite
import Effectful
import Effectful.Sqlite (SqlitePool (..))
import GHC.Conc (getNumCapabilities)
import Effectful.Concurrent.STM (TQueue, newTQueueIO)
import Moe.App.Logging (LogConfig (..), defaultLogConfig)
import Moe.Infra.Downloader.Adapter (DownloaderEnv, initDownloaderEnv)
import Moe.Infra.Setting.Effect (SettingEnv, initSettingEnv)
import Moe.Job.Subscription.Types (RssContext)
import Moe.Prelude
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
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
    settingEnv :: SettingEnv,
    httpManager :: Manager,
    dbPool :: SqlitePool,
    downloaderEnv :: DownloaderEnv,
    rssQueue :: TQueue RssContext
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
  appEnv <- parseAppEnv <$> liftIO (lookupEnv "ENV")
  port <- parsePort <$> liftIO (lookupEnv "PORT")
  dataFolder <- parseDataFolder <$> liftIO (lookupEnv "DATA_FOLDER")
  let logConfig = defaultLogConfig
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

mkMoeEnv :: (IOE :> es, FileSystem :> es, Concurrent :> es) => MoeConfig -> Eff es MoeEnv
mkMoeEnv config = do
  let settingPath = config.dataFolder </> "setting.json"
  settingEnv <- initSettingEnv settingPath
  httpManager <- liftIO $ newManager tlsManagerSettings
  dbPool <- mkDbPool (getDatabasePath' config)
  downloaderEnv <- initDownloaderEnv
  rssQueue <- newTQueueIO
  pure MoeEnv {config, settingEnv, httpManager, dbPool, downloaderEnv, rssQueue}

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


getDatabasePath :: MoeEnv -> FilePath
getDatabasePath env = getDatabasePath' env.config

getDatabasePath' :: MoeConfig -> FilePath
getDatabasePath' cfg = cfg.dataFolder </> "moe-bangumi.db"
