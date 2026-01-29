module Moe.App.Env
  ( MoeEnv (..),
    MetadataConfig (..),
    LogConfig (..),
    LogDestination (..),
    defaultMoeEnv,
  )
where

import Moe.App.Logging (LogConfig (..), LogDestination (..), defaultLogConfig)

data MetadataConfig = MetadataConfig
  { tmdbApiKey :: Text,
    bgmtvUserAgent :: Text
  }
  deriving stock (Eq, Show)

data MoeEnv = MoeEnv
  { databasePath :: FilePath,
    metadataConfig :: MetadataConfig,
    logConfig :: LogConfig
  }
  deriving stock (Eq, Show)

defaultMoeEnv :: MoeEnv
defaultMoeEnv =
  MoeEnv
    { databasePath = "moe-bangumi.db",
      metadataConfig =
        MetadataConfig
          { tmdbApiKey = "",
            bgmtvUserAgent = "moe-bangumi/0.1.0"
          },
      logConfig = defaultLogConfig
    }
