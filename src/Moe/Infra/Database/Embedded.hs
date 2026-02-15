-- | Compile-time embedded migration files.
-- Separate module so TH runs in its own compilation unit.
--
-- NOTE: Adding new migration files to @migrations/@ requires touching
-- this file or cleaning build artifacts to trigger recompilation,
-- because GHC does not track directory listings for TH dependencies.
module Moe.Infra.Database.Embedded
  ( embeddedMigrations,
  )
where

import Data.FileEmbed (embedDir)
import Moe.Prelude

-- | All SQL migration files from @migrations/@ embedded at compile time.
embeddedMigrations :: [(FilePath, ByteString)]
embeddedMigrations = $(embedDir "migrations")
