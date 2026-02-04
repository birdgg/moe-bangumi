module Moe.Domain.Setting.Types
  ( UserPreference (..),
    DownloaderConfig (..),
    FilterConfig (..),
    NotificationConfig (..),
    TMDBConfig (..),
    defaultUserPreference,
    defaultFilterConfig,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToSchema)
import Moe.Prelude

data DownloaderConfig = DownloaderConfig
  { url :: Text,
    username :: Text,
    password :: Text,
    savePath :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data FilterConfig = FilterConfig
  { globalRssFilter :: [Text],
    subtitleGroupPriority :: [Text]
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
  { apiKey :: Text,
    language :: Maybe Text
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
      filter = Just defaultFilterConfig,
      notification = Nothing,
      tmdb = Nothing
    }

defaultFilterConfig :: FilterConfig
defaultFilterConfig =
  FilterConfig
    { globalRssFilter =
        [ "720[Pp]",
          "\\d-\\d",
          "合集"
        ],
      subtitleGroupPriority = []
    }
