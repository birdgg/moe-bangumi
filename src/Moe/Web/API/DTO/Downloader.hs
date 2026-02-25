-- | DTOs for downloader test endpoint.
module Moe.Web.API.DTO.Downloader
  ( TestDownloaderRequest (..),
    TestDownloaderResponse (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Moe.Prelude

-- | Request to test a qBittorrent connection.
data TestDownloaderRequest = TestDownloaderRequest
  { url :: Text,
    username :: Text,
    password :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

-- | Response from a qBittorrent connection test.
data TestDownloaderResponse = TestDownloaderResponse
  { success :: Bool,
    version :: Maybe Text,
    message :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)
