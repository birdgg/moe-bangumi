-- | Self-update system for downloading and replacing the running binary.
module Moe.Infra.Update
  ( -- * Effect
    Update (..),
    checkForUpdate,
    performUpdate,

    -- * Types
    AboutInfo (..),
    UpdateClientError (..),
    PlatformInfo (..),
    Platform (..),
    Arch (..),
    detectPlatform,

    -- * Interpreter
    runUpdateGitHub,
    UpdateEnv,
    initUpdateEnv,
  )
where

import Moe.Infra.Update.Adapter (UpdateEnv, initUpdateEnv, runUpdateGitHub)
import Moe.Infra.Update.Effect
