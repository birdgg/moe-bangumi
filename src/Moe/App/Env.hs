module Moe.App.Env
  ( AppEnv (..),
    defaultAppEnv,
  )
where

data AppEnv = AppEnv
  { databasePath :: FilePath
  }
  deriving stock (Eq, Show)

defaultAppEnv :: AppEnv
defaultAppEnv =
  AppEnv
    { databasePath = "moe-bangumi.db"
    }
