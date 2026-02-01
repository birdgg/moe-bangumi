module Moe.Web.API.Routes where

import Data.Text (Text)
import Data.Time.Calendar (Year)
import GHC.Generics (Generic)
import Moe.Domain.Bangumi.Types (Season)
import Moe.Domain.Setting.Types (UserPreference)
import Moe.Web.API.Bangumi.Types (BangumiResponse)
import Moe.Web.API.Calendar.Types (CalendarResponse)
import Moe.Web.API.Setting.Types (SettingResponse)
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
          :> Get '[JSON] CalendarResponse,
    getSetting ::
      mode
        :- "settings"
          :> Get '[JSON] SettingResponse,
    updateSetting ::
      mode
        :- "settings"
          :> ReqBody '[JSON] UserPreference
          :> Put '[JSON] SettingResponse
  }
  deriving stock (Generic)
