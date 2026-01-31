module Moe.Domain.Setting.Types
  ( UserPreference (..),
    DownloaderConfig (..),
    FilterConfig (..),
    NotificationConfig (..),
    TMDBConfig (..),
    defaultUserPreference,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import GHC.Generics (Generic)

data DownloaderConfig = DownloaderConfig
  { url :: Text,
    username :: Text,
    password :: Text,
    savePath :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data FilterConfig = FilterConfig
  { globalRssFilter :: [Text]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data NotificationConfig = NotificationConfig
  { botToken :: Text,
    chatId :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data TMDBConfig = TMDBConfig
  { apiKey :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data UserPreference = UserPreference
  { downloader :: Maybe DownloaderConfig,
    filter :: Maybe FilterConfig,
    notification :: Maybe NotificationConfig,
    tmdb :: Maybe TMDBConfig
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

defaultUserPreference :: UserPreference
defaultUserPreference =
  UserPreference
    { downloader = Nothing,
      filter = Nothing,
      notification = Nothing,
      tmdb = Nothing
    }
