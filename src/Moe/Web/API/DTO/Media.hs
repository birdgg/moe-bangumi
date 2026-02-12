-- | DTOs for media server endpoints.
module Moe.Web.API.DTO.Media
  ( TestMediaRequest (..),
    TestMediaResponse (..),
    ImportLibraryRequest (..),
    ImportResult (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToSchema)
import Moe.Prelude

-- | Request to test an Emby connection.
data TestMediaRequest = TestMediaRequest
  { url :: Text,
    apiKey :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToSchema)

-- | Response from an Emby connection test.
data TestMediaResponse = TestMediaResponse
  { success :: Bool,
    serverName :: Maybe Text,
    message :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, ToSchema)

-- | Request to import a media library.
data ImportLibraryRequest = ImportLibraryRequest
  { url :: Text,
    apiKey :: Text,
    libraryId :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToSchema)

-- | Result of a library import operation.
data ImportResult = ImportResult
  { imported :: Int,
    failed :: Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, ToSchema)
