module Moe.Libs.Sqlite.Types
  ( SqlitePool (..),
    SqliteDb (..),
  )
where

import Data.Pool (Pool)
import Database.SQLite.Simple (Connection)
import Relude (FilePath)

newtype SqlitePool = SqlitePool {getPool :: Pool Connection}

data SqliteDb
  = DbFile FilePath
  | DbPool SqlitePool
