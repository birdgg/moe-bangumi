module Moe.Web.API.Routes where

import Data.Time.Calendar (Day, Year)
import Moe.Domain.Bangumi (Season)
import Moe.Domain.Setting (UserPreference)
import Moe.Prelude
import Moe.Web.API.DTO.Calendar (CalendarEntry)
import Moe.Web.API.DTO.Downloader (TestDownloaderRequest, TestDownloaderResponse)
import Moe.Web.API.DTO.Log (LogsResponse)
import Moe.Web.API.DTO.Notification (TestNotificationRequest, TestNotificationResponse)
import Moe.Web.API.DTO.Rss (DownloadTorrentRequest, RssSearchResult)
import Moe.Web.API.DTO.Bangumi (MikanSearchResultDTO, TmdbSearchResult, UpdateBangumiTmdbIdRequest)
import Moe.Web.API.DTO.Tracking (CreateTrackingRequest, TrackingResponse, TrackingWithBangumiResponse, UpdateTrackingRequest)
import Moe.Web.API.DTO.Update (AboutResponse, UpdateResponse)
import Servant

type Routes = "api" :> NamedRoutes Routes'

data Routes' mode = Routes'
  { health :: mode :- "health" :> Get '[JSON] Text,
    about ::
      mode
        :- "about"
          :> Get '[JSON] AboutResponse,
    update ::
      mode
        :- "update"
          :> Post '[JSON] UpdateResponse,
    calendar ::
      mode
        :- "calendar"
          :> QueryParam' '[Required, Strict] "year" Year
          :> QueryParam' '[Required, Strict] "season" Season
          :> Get '[JSON] [CalendarEntry],
    getSetting ::
      mode
        :- "settings"
          :> Get '[JSON] UserPreference,
    updateSetting ::
      mode
        :- "settings"
          :> ReqBody '[JSON] UserPreference
          :> Put '[JSON] UserPreference,
    listTracking ::
      mode
        :- "tracking"
          :> Get '[JSON] [TrackingResponse],
    listTrackingWithBangumi ::
      mode
        :- "tracking"
          :> "bangumis"
          :> Get '[JSON] [TrackingWithBangumiResponse],
    getTracking ::
      mode
        :- "tracking"
          :> Capture "id" Int64
          :> Get '[JSON] TrackingResponse,
    createTracking ::
      mode
        :- "tracking"
          :> ReqBody '[JSON] CreateTrackingRequest
          :> Post '[JSON] TrackingResponse,
    updateTracking ::
      mode
        :- "tracking"
          :> Capture "id" Int64
          :> ReqBody '[JSON] UpdateTrackingRequest
          :> Put '[JSON] TrackingResponse,
    deleteTracking ::
      mode
        :- "tracking"
          :> Capture "id" Int64
          :> Delete '[JSON] NoContent,
    searchRss ::
      mode
        :- "rss"
          :> "search"
          :> QueryParam' '[Required, Strict] "keyword" Text
          :> Get '[JSON] [RssSearchResult],
    downloadTorrent ::
      mode
        :- "rss"
          :> "download"
          :> ReqBody '[JSON] DownloadTorrentRequest
          :> Post '[JSON] NoContent,
    testDownloader ::
      mode
        :- "downloader"
          :> "test"
          :> ReqBody '[JSON] TestDownloaderRequest
          :> Post '[JSON] TestDownloaderResponse,
    testNotification ::
      mode
        :- "notification"
          :> "test"
          :> ReqBody '[JSON] TestNotificationRequest
          :> Post '[JSON] TestNotificationResponse,
    searchTmdb ::
      mode
        :- "bangumi"
          :> "search-tmdb"
          :> QueryParam' '[Required, Strict] "keyword" Text
          :> QueryParam "year" Year
          :> Get '[JSON] [TmdbSearchResult],
    searchMikan ::
      mode
        :- "bangumi"
          :> "search-mikan"
          :> QueryParam' '[Required, Strict] "keyword" Text
          :> Get '[JSON] [MikanSearchResultDTO],
    updateBangumiTmdbId ::
      mode
        :- "bangumi"
          :> Capture "id" Int64
          :> "tmdb-id"
          :> ReqBody '[JSON] UpdateBangumiTmdbIdRequest
          :> Put '[JSON] NoContent,
    getBangumiEpisodeOffset ::
      mode
        :- "bangumi"
          :> Capture "id" Int64
          :> "episode-offset"
          :> Get '[JSON] Word32,
    getLogs ::
      mode
        :- "logs"
          :> QueryParam' '[Required, Strict] "date" Day
          :> QueryParam "page" Word32
          :> QueryParam "pageSize" Word32
          :> Get '[JSON] LogsResponse
  }
  deriving stock (Generic)
