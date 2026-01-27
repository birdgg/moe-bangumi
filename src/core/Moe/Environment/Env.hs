module Moe.Environment.Env
  ( MoeEnv (..),
    defaultMoeEnv,
  )
where

data MoeEnv = MoeEnv
  { databasePath :: FilePath
  }
  deriving stock (Eq, Show)

defaultMoeEnv :: MoeEnv
defaultMoeEnv =
  MoeEnv
    { databasePath = "moe-bangumi.db"
    }
