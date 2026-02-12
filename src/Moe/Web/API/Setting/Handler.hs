module Moe.Web.API.Setting.Handler
  ( handleGetSetting,
    handleUpdateSetting,
  )
where

import Moe.Domain.Setting (UserPreference)
import Moe.Infra.Setting.Effect (getSetting, saveSetting)
import Moe.Prelude
import Moe.Web.Types (ServerEff)

-- | Return the full user preference.
handleGetSetting :: ServerEff UserPreference
handleGetSetting = getSetting

-- | Save and return the updated user preference.
handleUpdateSetting :: UserPreference -> ServerEff UserPreference
handleUpdateSetting newSetting = do
  saveSetting newSetting
  pure newSetting
