module Moe.Effect.Setting
  ( Setting (..),
    getSetting,
    saveSetting,
  )
where

import Effectful
import Effectful.TH (makeEffect)
import Moe.Domain.Setting.Types (UserPreference)

data Setting :: Effect where
  GetSetting :: Setting m UserPreference
  SaveSetting :: UserPreference -> Setting m ()

makeEffect ''Setting
