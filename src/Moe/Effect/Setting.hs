module Moe.Effect.Setting
  ( Setting (..),
    getSetting,
    saveSetting,
    runSettingTVar,
  )
where

import Control.Concurrent.STM (TVar, atomically, readTVarIO, writeTVar)
import Data.Aeson (encodeFile)
import Effectful
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.TH (makeEffect)
import Moe.Domain.Setting.Types (UserPreference)

data Setting :: Effect where
  GetSetting :: Setting m UserPreference
  SaveSetting :: UserPreference -> Setting m ()

makeEffect ''Setting

runSettingTVar ::
  (IOE :> es) =>
  TVar UserPreference ->
  FilePath ->
  Eff (Setting : es) a ->
  Eff es a
runSettingTVar settingVar settingPath = interpret $ \_ -> \case
  GetSetting -> liftIO $ readTVarIO settingVar
  SaveSetting setting -> liftIO $ do
    atomically $ writeTVar settingVar setting
    encodeFile settingPath setting
