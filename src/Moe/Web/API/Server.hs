module Moe.Web.API.Server where

import Data.Time.Calendar (Year)
import Moe.App.BangumiSync (syncBangumiSeason)
import Moe.Domain.Bangumi.Types (BangumiSeason (..), Season)
import Moe.Domain.Setting.Types (UserPreference)
import Moe.Effect.Setting (getSetting, saveSetting)
import Moe.Web.API.Routes qualified as API
import Moe.Web.API.Types (BangumiResponse, SettingResponse, toBangumiResponse, toSettingResponse)
import Moe.Web.Types
import Servant

apiServer :: ServerT API.Routes MoeEff
apiServer =
  API.Routes'
    { health = pure "ok",
      bangumiSeason = handleBangumiSeason,
      getSetting = handleGetSetting,
      updateSetting = handleUpdateSetting
    }

handleBangumiSeason :: Year -> Season -> MoeEff [BangumiResponse]
handleBangumiSeason year season = do
  let bs = BangumiSeason year season
  bangumis <- syncBangumiSeason bs
  pure $ map toBangumiResponse bangumis

handleGetSetting :: MoeEff SettingResponse
handleGetSetting = do
  pref <- getSetting
  pure $ toSettingResponse pref

handleUpdateSetting :: UserPreference -> MoeEff SettingResponse
handleUpdateSetting newSetting = do
  saveSetting newSetting
  pure $ toSettingResponse newSetting
