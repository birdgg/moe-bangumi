module Moe.App.Env
  ( MoeEnv (..),
    AppEnv (..),
    defaultMoeEnv,
    parseMoeEnv,
    getDatabasePath,
    getSettingPath,
  )
where

import Data.Text.Display (Display (..))
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
    dataFolder :: FilePath
  }
  deriving stock (Eq, Show)

defaultMoeEnv :: MoeEnv
defaultMoeEnv =
  MoeEnv
    { appEnv = Development,
      port = 3000,
      dataFolder = "./data"
    }

parseMoeEnv :: IO MoeEnv
parseMoeEnv = do
  appEnv <- parseAppEnv <$> lookupEnv "MOE_ENV"
  port <- parsePort <$> lookupEnv "MOE_PORT"
  dataFolder <- parseDataFolder <$> lookupEnv "MOE_DATA_FOLDER"
  pure MoeEnv {appEnv, port, dataFolder}
  where
    parseAppEnv :: Maybe String -> AppEnv
    parseAppEnv (Just "prod") = Production
    parseAppEnv (Just "production") = Production
    parseAppEnv _ = Development

    parsePort :: Maybe String -> Int
    parsePort mstr = fromMaybe 3000 (mstr >>= readMaybe)

    parseDataFolder :: Maybe String -> FilePath
    parseDataFolder = fromMaybe "./data"

getDatabasePath :: MoeEnv -> FilePath
getDatabasePath env = dataFolder env </> "moe-bangumi.db"

getSettingPath :: MoeEnv -> FilePath
getSettingPath env = dataFolder env </> "setting.json"
