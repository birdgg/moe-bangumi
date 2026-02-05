module Moe.Web.API.DTO.Setting
  ( SettingResponse (..),
    toSettingResponse,
    DownloaderConfigResponse (..),
    FilterConfigResponse (..),
    WashingConfigResponse (..),
    NotificationConfigResponse (..),
    TMDBConfigResponse (..),
  )
where

import Data.Aeson (ToJSON)
import Data.OpenApi (ToSchema)
import Moe.Domain.Bangumi.Internal.Group (Group)
import Moe.Domain.Setting.Types qualified as Setting
import Moe.Prelude

data SettingResponse = SettingResponse
  { downloader :: Maybe DownloaderConfigResponse,
    filter :: Maybe FilterConfigResponse,
    washing :: Maybe WashingConfigResponse,
    notification :: Maybe NotificationConfigResponse,
    tmdb :: Maybe TMDBConfigResponse
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, ToSchema)

data DownloaderConfigResponse = DownloaderConfigResponse
  { url :: Text,
    username :: Text,
    savePath :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, ToSchema)

data FilterConfigResponse = FilterConfigResponse
  { globalRssFilter :: [Text]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, ToSchema)

data WashingConfigResponse = WashingConfigResponse
  { groupPriority :: [Group]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, ToSchema)

data NotificationConfigResponse = NotificationConfigResponse
  { chatId :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, ToSchema)

data TMDBConfigResponse = TMDBConfigResponse
  { apiKeyConfigured :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, ToSchema)

toSettingResponse :: Setting.UserPreference -> SettingResponse
toSettingResponse pref =
  SettingResponse
    { downloader = toDownloaderResponse <$> pref.downloader,
      filter = toFilterResponse <$> pref.filter,
      washing = toWashingResponse <$> pref.washing,
      notification = toNotificationResponse <$> pref.notification,
      tmdb = toTMDBResponse <$> pref.tmdb
    }

toDownloaderResponse :: Setting.DownloaderConfig -> DownloaderConfigResponse
toDownloaderResponse cfg =
  DownloaderConfigResponse
    { url = cfg.url,
      username = cfg.username,
      savePath = cfg.savePath
    }

toFilterResponse :: Setting.FilterConfig -> FilterConfigResponse
toFilterResponse cfg =
  FilterConfigResponse
    { globalRssFilter = cfg.globalRssFilter
    }

toWashingResponse :: Setting.WashingConfig -> WashingConfigResponse
toWashingResponse cfg =
  WashingConfigResponse
    { groupPriority = cfg.groupPriority
    }

toNotificationResponse :: Setting.NotificationConfig -> NotificationConfigResponse
toNotificationResponse cfg =
  NotificationConfigResponse
    { chatId = cfg.chatId
    }

toTMDBResponse :: Setting.TMDBConfig -> TMDBConfigResponse
toTMDBResponse cfg =
  TMDBConfigResponse
    { apiKeyConfigured = cfg.apiKey /= ""
    }
