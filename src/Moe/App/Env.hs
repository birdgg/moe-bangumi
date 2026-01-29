module Moe.App.Env
  ( MoeEnv (..),
    LogConfig (..),
    LogDestination (..),
    AppEnv (..),
    defaultMoeEnv,
    getDatabasePath,
    getSettingPath,
  )
where

import Data.Text.Display (Display (..))
import Moe.App.Logging (LogConfig (..), LogDestination (..), defaultLogConfig)
import System.FilePath ((</>))

data AppEnv
  = Development
  | Production
  deriving stock (Eq, Show, Enum, Bounded)

instance Display AppEnv where
  displayBuilder Development = "dev"
  displayBuilder Production = "prod"

data MoeEnv = MoeEnv
  { appEnv :: AppEnv,
    port :: Int,
    dataFolder :: FilePath,
    tmdbApiKey :: Text,
    bgmtvUserAgent :: Text,
    logConfig :: LogConfig
  }
  deriving stock (Eq, Show)

defaultMoeEnv :: MoeEnv
defaultMoeEnv =
  MoeEnv
    { appEnv = Development,
      port = 3000,
      dataFolder = "./data",
      tmdbApiKey = "",
      bgmtvUserAgent = "moe-bangumi/0.1.0",
      logConfig = defaultLogConfig
    }

getDatabasePath :: MoeEnv -> FilePath
getDatabasePath env = dataFolder env </> "moe-bangumi.db"

getSettingPath :: MoeEnv -> FilePath
getSettingPath env = dataFolder env </> "setting.json"
