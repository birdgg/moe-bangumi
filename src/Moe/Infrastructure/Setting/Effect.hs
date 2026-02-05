module Moe.Infrastructure.Setting.Effect
  ( Setting (..),
    getSetting,
    saveSetting,
    runSettingTVar,
    loadSettingFromFile,
  )
where

import Data.Aeson (eitherDecode, encode)
import Effectful
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.FileSystem (FileSystem, doesFileExist)
import Effectful.FileSystem.IO.ByteString.Lazy qualified as LBS
import Effectful.TH (makeEffect)
import Moe.Domain.Setting.Types (UserPreference, defaultUserPreference)
import Moe.Prelude

data Setting :: Effect where
  GetSetting :: Setting m UserPreference
  SaveSetting :: UserPreference -> Setting m ()

makeEffect ''Setting

runSettingTVar ::
  (FileSystem :> es, IOE :> es) =>
  TVar UserPreference ->
  FilePath ->
  Eff (Setting : es) a ->
  Eff es a
runSettingTVar settingVar settingPath = interpret $ \_ -> \case
  GetSetting -> liftIO $ readTVarIO settingVar
  SaveSetting setting -> do
    liftIO $ atomically $ writeTVar settingVar setting
    LBS.writeFile settingPath (encode setting)

-- | Load user preference from file, falling back to default.
loadSettingFromFile :: (FileSystem :> es) => FilePath -> Eff es UserPreference
loadSettingFromFile path = do
  exists <- doesFileExist path
  if exists
    then fromRight defaultUserPreference . eitherDecode <$> LBS.readFile path
    else pure defaultUserPreference
