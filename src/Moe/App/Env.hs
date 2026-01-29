module Moe.App.Env
  ( MoeEnv (..),
    MetadataConfig (..),
    defaultMoeEnv,
  )
where

data MetadataConfig = MetadataConfig
  { tmdbApiKey :: Text,
    bgmtvUserAgent :: Text
  }
  deriving stock (Eq, Show)

data MoeEnv = MoeEnv
  { databasePath :: FilePath,
    metadataConfig :: MetadataConfig
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
          }
    }
