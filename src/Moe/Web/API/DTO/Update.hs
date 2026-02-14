-- | DTOs for update API endpoints.
module Moe.Web.API.DTO.Update
  ( AboutResponse (..),
    UpdateResponse (..),
    toAboutResponse,
  )
where

import Data.Aeson (ToJSON)
import Data.OpenApi (ToSchema)
import Data.Time (UTCTime)
import Moe.Infra.Update.Types (AboutInfo (..), PlatformInfo (..))
import Moe.Prelude

-- | Version and update information response.
data AboutResponse = AboutResponse
  { version :: Text,
    latest :: Text,
    needUpdate :: Bool,
    autoUpdate :: Bool,
    changelog :: Text,
    publishedAt :: Maybe UTCTime,
    platform :: Text,
    arch :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, ToSchema)

-- | Update operation response.
data UpdateResponse = UpdateResponse
  { success :: Bool,
    message :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, ToSchema)

-- | Convert internal AboutInfo + PlatformInfo to API response.
toAboutResponse :: AboutInfo -> PlatformInfo -> AboutResponse
toAboutResponse about pinfo =
  AboutResponse
    { version = about.currentVersion,
      latest = about.latestVersion,
      needUpdate = about.needUpdate,
      autoUpdate = about.autoUpdate,
      changelog = about.changelog,
      publishedAt = about.publishedAt,
      platform = toText pinfo.platform,
      arch = toText pinfo.arch
    }
