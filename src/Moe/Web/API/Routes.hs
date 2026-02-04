module Moe.Web.API.Routes where

import Data.Time.Calendar (Year)
import Moe.Domain.Bangumi.Types (Season)
import Moe.Domain.Setting.Types (UserPreference)
import Moe.Prelude
import Moe.Web.API.DTO.Bangumi (BangumiResponse)
import Moe.Web.API.DTO.Calendar (CalendarEntry)
import Moe.Web.API.DTO.Setting (SettingResponse)
import Moe.Web.API.DTO.Tracking (CreateTrackingRequest, TrackingResponse, TrackingWithBangumiResponse, UpdateTrackingRequest)
import Servant

type Routes = "api" :> NamedRoutes Routes'

data Routes' mode = Routes'
  { health :: mode :- "health" :> Get '[JSON] Text,
    bangumiSeason ::
      mode
        :- "bangumi"
          :> "season"
          :> QueryParam' '[Required, Strict] "year" Year
          :> QueryParam' '[Required, Strict] "season" Season
          :> QueryParam "force" Bool
          :> Get '[JSON] [BangumiResponse],
    calendar ::
      mode
        :- "calendar"
          :> QueryParam' '[Required, Strict] "year" Year
          :> QueryParam' '[Required, Strict] "season" Season
          :> Get '[JSON] [CalendarEntry],
    getSetting ::
      mode
        :- "settings"
          :> Get '[JSON] SettingResponse,
    updateSetting ::
      mode
        :- "settings"
          :> ReqBody '[JSON] UserPreference
          :> Put '[JSON] SettingResponse,
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
          :> Delete '[JSON] NoContent
  }
  deriving stock (Generic)
