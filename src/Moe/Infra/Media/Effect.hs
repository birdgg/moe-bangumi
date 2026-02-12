-- | Abstract Media effect for media server integration.
--
-- Provides a server-agnostic interface that can be implemented
-- by Emby, Jellyfin, or other media servers.
module Moe.Infra.Media.Effect
  ( -- * Effect
    Media (..),

    -- * Operations
    testMediaConnection,
    getLibraries,
    getLibraryItems,

    -- * Re-exports
    module Moe.Infra.Media.Types,
  )
where

import Effectful
import Effectful.TH (makeEffect)
import Moe.Infra.Media.Types
import Moe.Prelude

-- | Abstract media server effect.
data Media :: Effect where
  -- | Test connection with explicit credentials (url, apiKey).
  --   Returns Right serverName on success, Left error message on failure.
  TestMediaConnection :: Text -> Text -> Media m (Either Text Text)
  -- | Get all media libraries from the configured server.
  GetLibraries :: Media m [MediaLibrary]
  -- | Get all items from a specific library by its ID.
  GetLibraryItems :: Text -> Media m [MediaItem]

type instance DispatchOf Media = 'Dynamic

makeEffect ''Media
