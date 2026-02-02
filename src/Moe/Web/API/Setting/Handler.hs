module Moe.Web.API.Setting.Handler
  ( handleGetSetting,
    handleUpdateSetting,
  )
where

import Moe.Domain.Setting.Types (UserPreference)
import Moe.Infrastructure.Setting.Effect (getSetting, saveSetting)
import Moe.Web.API.DTO.Setting (SettingResponse, toSettingResponse)
import Moe.Web.Types (MoeEff)

handleGetSetting :: MoeEff SettingResponse
handleGetSetting = toSettingResponse <$> getSetting

handleUpdateSetting :: UserPreference -> MoeEff SettingResponse
handleUpdateSetting newSetting = do
  saveSetting newSetting
  pure $ toSettingResponse newSetting
