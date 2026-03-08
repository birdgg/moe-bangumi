-- | Abstract Media effect for triggering media library refreshes.
module Moe.Infra.Media.Effect
  ( -- * Effect
    Media (..),

    -- * Operations
    refreshLibrary,
  )
where

import Moe.Prelude

-- | Abstract media library effect.
data Media :: Effect where
  -- | Trigger a media library refresh (e.g. Plex scan).
  RefreshLibrary :: Media m ()

makeEffect ''Media
