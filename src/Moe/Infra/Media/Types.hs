-- | Types for media server operations.
module Moe.Infra.Media.Types
  ( -- * Data types
    MediaLibrary (..),
    MediaItem (..),
    MediaClientError (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToSchema)
import Data.Text.Display (Display (..))
import Moe.Prelude

-- | Structured media client errors.
data MediaClientError
  = MediaNetworkError Text
  | MediaHttpError Int
  | MediaParseError Text
  | MediaInvalidUrl Text
  | MediaConfigError Text
  deriving stock (Show, Eq)

instance Display MediaClientError where
  displayBuilder = \case
    MediaNetworkError msg -> "Media network error: " <> displayBuilder msg
    MediaHttpError code -> "Media HTTP " <> displayBuilder (show @Text code)
    MediaParseError msg -> "Media parse error: " <> displayBuilder msg
    MediaInvalidUrl url -> "Media invalid URL: " <> displayBuilder url
    MediaConfigError msg -> "Media: " <> displayBuilder msg

-- | A media library from the server.
data MediaLibrary = MediaLibrary
  { libraryId :: Text,
    libraryName :: Text,
    collectionType :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

-- | A media item from the server.
data MediaItem = MediaItem
  { itemName :: Text,
    itemType :: Text,
    premiereDate :: Maybe Text,
    providerIds :: Map Text Text,
    episodeCount :: Maybe Word32,
    playedCount :: Maybe Word32
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)
