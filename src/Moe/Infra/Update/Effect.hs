-- | Abstract Update effect for self-update operations.
module Moe.Infra.Update.Effect
  ( -- * Effect
    Update (..),

    -- * Operations
    checkForUpdate,
    performUpdate,

    -- * Re-exports from Types
    module Moe.Infra.Update.Types,
  )
where

import Effectful
import Effectful.TH (makeEffect)
import Moe.Infra.Update.Types

-- | Abstract update effect.
data Update :: Effect where
  -- | Check for new version (cached). Always returns current version info.
  CheckForUpdate :: Update m AboutInfo
  -- | Download, verify, replace executable, and exit with code 0.
  PerformUpdate :: Update m ()

makeEffect ''Update
