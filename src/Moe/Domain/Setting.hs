module Moe.Domain.Setting
  ( UserPreference (..),
    DownloaderConfig (..),
    FilterConfig (..),
    WashingConfig (..),
    NotificationConfig (..),
    TMDBConfig (..),
    Regex,
    defaultUserPreference,
    defaultDownloaderConfig,
    defaultFilterConfig,
    defaultWashingConfig,
    defaultNotificationConfig,
    defaultTMDBConfig,
    defaultSubtitlePriority,
    defaultGroupPriority,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToSchema)
import Moe.Domain.Shared.Group (Group (..), GroupName (..))
import Moe.Domain.Shared.Subtitle (Subtitle (..), SubtitleList)
import Moe.Prelude

type Regex = Text

data DownloaderConfig = DownloaderConfig
  { url :: Text,
    username :: Text,
    password :: Text,
    savePath :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

-- | Default downloader config with empty fields.
defaultDownloaderConfig :: DownloaderConfig
defaultDownloaderConfig =
  DownloaderConfig
    { url = "",
      username = "",
      password = "",
      savePath = ""
    }

data FilterConfig = FilterConfig
  { globalRssFilter :: [Regex]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

defaultFilterConfig :: FilterConfig
defaultFilterConfig =
  FilterConfig
    { globalRssFilter =
        [ "720[Pp]",
          "\\d-\\d",
          "合集"
        ]
    }

data WashingConfig = WashingConfig
  { groupPriority :: [Group],
    subtitlePriority :: [SubtitleList]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

defaultWashingConfig :: WashingConfig
defaultWashingConfig =
  WashingConfig
    { groupPriority = defaultGroupPriority,
      subtitlePriority = defaultSubtitlePriority
    }

defaultSubtitlePriority :: [SubtitleList]
defaultSubtitlePriority = [[CHS, JPN], [CHS], [CHT, JPN], [CHT]]

defaultGroupPriority :: [Group]
defaultGroupPriority =
  [ Group (GroupName "SweetSub") [],
    Group (GroupName "千夏字幕组") [],
    Group (GroupName "拨雪寻春") ["❀拨雪寻春❀"],
    Group (GroupName "喵萌奶茶屋") [],
    Group (GroupName "LoliHouse") [],
    Group (GroupName "北宇治") ["北宇治字幕组"],
    Group (GroupName "诸神字幕组") [],
    Group (GroupName "霜庭云花") [],
    Group (GroupName "桜都字幕组") [],
    Group (GroupName "澄空学园") []
  ]

data NotificationConfig = NotificationConfig
  { botToken :: Text,
    chatId :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

-- | Default notification config with empty fields.
defaultNotificationConfig :: NotificationConfig
defaultNotificationConfig =
  NotificationConfig
    { botToken = "",
      chatId = ""
    }

data TMDBConfig = TMDBConfig
  { apiKey :: Text,
    language :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

-- | Default TMDB config with empty fields.
defaultTMDBConfig :: TMDBConfig
defaultTMDBConfig =
  TMDBConfig
    { apiKey = "",
      language = "zh-CN"
    }

data UserPreference = UserPreference
  { downloader :: DownloaderConfig,
    filter :: FilterConfig,
    washing :: WashingConfig,
    notification :: NotificationConfig,
    tmdb :: TMDBConfig
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

defaultUserPreference :: UserPreference
defaultUserPreference =
  UserPreference
    { downloader = defaultDownloaderConfig,
      filter = defaultFilterConfig,
      washing = defaultWashingConfig,
      notification = defaultNotificationConfig,
      tmdb = defaultTMDBConfig
    }
