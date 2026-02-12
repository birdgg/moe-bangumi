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
import Moe.Domain.Shared.Group (Group)
import Moe.Domain.Shared.Subtitle (SubtitleList)
import Moe.Domain.Setting qualified as Setting
import Moe.Prelude

data SettingResponse = SettingResponse
  { downloader :: Maybe DownloaderConfigResponse,
    filter :: FilterConfigResponse,
    washing :: WashingConfigResponse,
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
  { groupPriority :: [Group],
    subtitlePriority :: [SubtitleList]
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
    { downloader = Just $ toDownloaderResponse pref.downloader,
      filter = toFilterResponse pref.filter,
      washing = toWashingResponse pref.washing,
      notification =
        if pref.notification.botToken == ""
          then Nothing
          else Just $ toNotificationResponse pref.notification,
      tmdb =
        if pref.tmdb.apiKey == ""
          then Nothing
          else Just $ toTMDBResponse pref.tmdb
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
    { groupPriority = cfg.groupPriority,
      subtitlePriority = cfg.subtitlePriority
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
