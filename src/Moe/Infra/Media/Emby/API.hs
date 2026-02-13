-- | Emby API type definitions for servant-client.
module Moe.Infra.Media.Emby.API
  ( EmbyAPI,
    EmbyRoutes (..),
  )
where

import Moe.Infra.Media.Emby.Types (EmbyLibrary, EmbyUser, ItemsResponse, SystemInfo)
import Moe.Prelude
import Servant.API

-- | Emby API using NamedRoutes.
type EmbyAPI = "emby" :> NamedRoutes EmbyRoutes

-- | Emby API endpoints.
data EmbyRoutes mode = EmbyRoutes
  { -- | GET /emby/System/Info/Public
    systemInfo ::
      mode
        :- "System"
          :> "Info"
          :> "Public"
          :> QueryParam' '[Required, Strict] "api_key" Text
          :> Get '[JSON] SystemInfo,
    -- | GET /emby/Library/VirtualFolders
    virtualFolders ::
      mode
        :- "Library"
          :> "VirtualFolders"
          :> QueryParam' '[Required, Strict] "api_key" Text
          :> Get '[JSON] [EmbyLibrary],
    -- | GET /emby/Users
    users ::
      mode
        :- "Users"
          :> QueryParam' '[Required, Strict] "api_key" Text
          :> Get '[JSON] [EmbyUser],
    -- | GET /emby/Users/{userId}/Items
    items ::
      mode
        :- "Users"
          :> Capture "userId" Text
          :> "Items"
          :> QueryParam' '[Required, Strict] "ParentId" Text
          :> QueryParam' '[Required, Strict] "Recursive" Bool
          :> QueryParam' '[Required, Strict] "IncludeItemTypes" Text
          :> QueryParam' '[Required, Strict] "Fields" Text
          :> QueryParam' '[Required, Strict] "api_key" Text
          :> Get '[JSON] ItemsResponse
  }
  deriving stock (Generic)
