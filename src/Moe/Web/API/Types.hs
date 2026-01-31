module Moe.Web.API.Types
  ( BangumiResponse (..),
    toBangumiResponse,
    SettingResponse (..),
    toSettingResponse,
    UpdateSettingRequest (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Coerce (coerce)
import Data.Int (Int64)
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import Data.Text.Conversions (ToText (..))
import Data.Time.Calendar (Day)
import Data.Word (Word32)
import GHC.Generics (Generic)
import Moe.Domain.Bangumi.Types qualified as Types
import Moe.Domain.Setting.Types qualified as Setting

data BangumiResponse = BangumiResponse
  { id :: Maybe Int64,
    titleChs :: Text,
    titleJap :: Maybe Text,
    airDate :: Maybe Day,
    seasonNumber :: Maybe Word32,
    kind :: Text,
    posterUrl :: Maybe Text,
    bangumiSeason :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, ToSchema)

{- HLINT ignore toBangumiResponse "Redundant id" -}
toBangumiResponse :: Types.Bangumi -> BangumiResponse
toBangumiResponse b =
  BangumiResponse
    { id = fmap coerce b.id,
      titleChs = b.titleChs,
      titleJap = b.titleJap,
      airDate = b.airDate,
      seasonNumber = b.seasonNumber,
      kind = toText b.kind,
      posterUrl = b.posterUrl,
      bangumiSeason = Types.bangumiSeasonToText <$> Types.getBangumiSeason b
    }

data SettingResponse = SettingResponse
  { downloader :: Maybe DownloaderConfigResponse,
    filter :: Maybe FilterConfigResponse,
    notification :: Maybe NotificationConfigResponse,
    metadata :: Maybe MetadataConfigResponse
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

data NotificationConfigResponse = NotificationConfigResponse
  { chatId :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, ToSchema)

data MetadataConfigResponse = MetadataConfigResponse
  { tmdbApiKeyConfigured :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, ToSchema)

toSettingResponse :: Setting.UserPreference -> SettingResponse
toSettingResponse pref =
  SettingResponse
    { downloader = toDownloaderResponse <$> pref.downloader,
      filter = toFilterResponse <$> pref.filter,
      notification = toNotificationResponse <$> pref.notification,
      metadata = toMetadataResponse <$> pref.metadata
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

toNotificationResponse :: Setting.NotificationConfig -> NotificationConfigResponse
toNotificationResponse cfg =
  NotificationConfigResponse
    { chatId = cfg.chatId
    }

toMetadataResponse :: Setting.MetadataConfig -> MetadataConfigResponse
toMetadataResponse cfg =
  MetadataConfigResponse
    { tmdbApiKeyConfigured = cfg.tmdbApiKey /= ""
    }

newtype UpdateSettingRequest = UpdateSettingRequest
  { setting :: Setting.UserPreference
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)
