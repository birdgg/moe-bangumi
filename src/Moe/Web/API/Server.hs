module Moe.Web.API.Server where

import Moe.Web.API.Bangumi.Handler (handleBangumiSeason)
import Moe.Web.API.Calendar.Handler (handleCalendar)
import Moe.Web.API.Routes qualified as API
import Moe.Web.API.Setting.Handler (handleGetSetting, handleUpdateSetting)
import Moe.Web.API.Tracking.Handler qualified as Tracking
import Moe.Web.Types (MoeEff)
import Servant (ServerT)

apiServer :: ServerT API.Routes MoeEff
apiServer =
  API.Routes'
    { health = pure "ok",
      bangumiSeason = handleBangumiSeason,
      calendar = handleCalendar,
      getSetting = handleGetSetting,
      updateSetting = handleUpdateSetting,
      listTracking = Tracking.handleListTracking,
      listTrackingWithBangumi = Tracking.handleListTrackingWithBangumi,
      getTracking = Tracking.handleGetTracking,
      createTracking = Tracking.handleCreateTracking,
      updateTracking = Tracking.handleUpdateTracking,
      deleteTracking = Tracking.handleDeleteTracking
    }
