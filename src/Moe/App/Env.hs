module Moe.App.Env
  ( MoeConfig (..),
    MoeEnv (..),
    AppEnv (..),
    SchedulerConfig (..),
    defaultMoeConfig,
    defaultSchedulerConfig,
    parseMoeConfig,
    mkMoeEnv,
    getDatabasePath,
    getSettingPath,
    getLogPath,
  )
where

import Control.Concurrent.STM (TVar, newTVarIO)
import Data.Aeson (eitherDecodeFileStrict)
import Data.Either (fromRight)
import Data.Maybe (fromMaybe)
import Data.Text.Display (Display (..))
import Moe.App.Logging (LogConfig (..), LogDestination (..), defaultLogConfig)
import Moe.Domain.Setting.Types (UserPreference, defaultUserPreference)
import Numeric.Natural (Natural)
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import Text.Read (readMaybe)

data AppEnv
  = Development
  | Production
  deriving stock (Eq, Show, Enum, Bounded)

instance Display AppEnv where
  displayBuilder Development = "dev"
  displayBuilder Production = "prod"

data SchedulerConfig = SchedulerConfig
  { rssSyncInterval :: Natural,
    bangumiDataSyncInterval :: Natural,
    cleanupInterval :: Natural,
    enableScheduler :: Bool
  }
  deriving stock (Eq, Show)

defaultSchedulerConfig :: SchedulerConfig
defaultSchedulerConfig =
  SchedulerConfig
    { rssSyncInterval = 300,
      bangumiDataSyncInterval = 86400,
      cleanupInterval = 3600,
      enableScheduler = True
    }

data MoeConfig = MoeConfig
  { appEnv :: AppEnv,
    port :: Int,
    dataFolder :: FilePath,
    logConfig :: LogConfig,
    schedulerConfig :: SchedulerConfig
  }
  deriving stock (Eq, Show)

data MoeEnv = MoeEnv
  { config :: MoeConfig,
    settingVar :: TVar UserPreference
  }

defaultMoeConfig :: MoeConfig
defaultMoeConfig =
  MoeConfig
    { appEnv = Development,
      port = 3000,
      dataFolder = "./data",
      logConfig = defaultLogConfig,
      schedulerConfig = defaultSchedulerConfig
    }

parseMoeConfig :: IO MoeConfig
parseMoeConfig = do
  appEnv <- parseAppEnv <$> lookupEnv "MOE_ENV"
  port <- parsePort <$> lookupEnv "MOE_PORT"
  dataFolder <- parseDataFolder <$> lookupEnv "MOE_DATA_FOLDER"
  logDest <- parseLogDest dataFolder <$> lookupEnv "MOE_LOG_DEST"
  schedulerConfig <- parseSchedulerConfig
  let logConfig = LogConfig {destination = logDest, logLevel = defaultLogConfig.logLevel}
  pure MoeConfig {appEnv, port, dataFolder, logConfig, schedulerConfig}
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

parseSchedulerConfig :: IO SchedulerConfig
parseSchedulerConfig = do
  rssSyncInterval <- parseNatural 300 <$> lookupEnv "MOE_RSS_SYNC_INTERVAL"
  bangumiDataSyncInterval <- parseNatural 86400 <$> lookupEnv "MOE_BANGUMI_SYNC_INTERVAL"
  cleanupInterval <- parseNatural 3600 <$> lookupEnv "MOE_CLEANUP_INTERVAL"
  enableScheduler <- parseBool True <$> lookupEnv "MOE_SCHEDULER_ENABLED"
  pure SchedulerConfig {rssSyncInterval, bangumiDataSyncInterval, cleanupInterval, enableScheduler}
  where
    parseNatural :: Natural -> Maybe String -> Natural
    parseNatural def mstr = fromMaybe def (mstr >>= readMaybe)

    parseBool :: Bool -> Maybe String -> Bool
    parseBool def Nothing = def
    parseBool _ (Just "true") = True
    parseBool _ (Just "1") = True
    parseBool _ (Just "false") = False
    parseBool _ (Just "0") = False
    parseBool def _ = def

mkMoeEnv :: MoeConfig -> IO MoeEnv
mkMoeEnv config = do
  let settingPath = config.dataFolder </> "setting.json"
  initialSetting <- loadSettingFromFile settingPath
  settingVar <- newTVarIO initialSetting
  pure MoeEnv {config, settingVar}

loadSettingFromFile :: FilePath -> IO UserPreference
loadSettingFromFile path = do
  exists <- doesFileExist path
  if exists
    then fromRight defaultUserPreference <$> eitherDecodeFileStrict path
    else pure defaultUserPreference

getDatabasePath :: MoeEnv -> FilePath
getDatabasePath env = getDatabasePath' env.config

getDatabasePath' :: MoeConfig -> FilePath
getDatabasePath' cfg = cfg.dataFolder </> "moe-bangumi.db"

getSettingPath :: MoeEnv -> FilePath
getSettingPath env = env.config.dataFolder </> "setting.json"

getLogPath :: MoeEnv -> FilePath
getLogPath env = env.config.dataFolder </> "moe-bangumi.log"
