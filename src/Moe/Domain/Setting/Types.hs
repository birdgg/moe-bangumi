module Moe.Domain.Setting.Types
  ( UserPreference (..),
    DownloaderConfig (..),
    FilterConfig (..),
    NotificationConfig (..),
    MetadataConfig (..),
    defaultUserPreference,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

data DownloaderConfig = DownloaderConfig
  { url :: Text,
    username :: Text,
    password :: Text,
    savePath :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data FilterConfig = FilterConfig
  { globalRssFilter :: [Text]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data NotificationConfig = NotificationConfig
  { botToken :: Text,
    chatId :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data MetadataConfig = MetadataConfig
  { tmdbApiKey :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data UserPreference = UserPreference
  { downloader :: Maybe DownloaderConfig,
    filter :: Maybe FilterConfig,
    notification :: Maybe NotificationConfig,
    metadata :: Maybe MetadataConfig
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

defaultUserPreference :: UserPreference
defaultUserPreference =
  UserPreference
    { downloader = Nothing,
      filter = Nothing,
      notification = Nothing,
      metadata = Nothing
    }
