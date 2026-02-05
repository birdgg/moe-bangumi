module Moe.Infrastructure.Setting.Effect
  ( Setting (..),
    getSetting,
    saveSetting,
    SettingEnv,
    initSettingEnv,
    runSetting,
  )
where

import Data.Aeson (eitherDecode, encode)
import Effectful
import Effectful.Concurrent (Concurrent)
import Effectful.Concurrent.STM qualified as STM
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

-- | Opaque setting environment.
data SettingEnv = SettingEnv
  { var :: TVar UserPreference,
    path :: FilePath
  }

-- | Initialize setting env by loading from file.
initSettingEnv :: (FileSystem :> es, Concurrent :> es) => FilePath -> Eff es SettingEnv
initSettingEnv path = do
  initial <- loadSettingFromFile path
  var <- STM.newTVarIO initial
  pure SettingEnv {var, path}

-- | Run Setting effect with pre-initialized env.
runSetting ::
  (FileSystem :> es, Concurrent :> es) =>
  SettingEnv ->
  Eff (Setting : es) a ->
  Eff es a
runSetting env = interpret $ \_ -> \case
  GetSetting -> STM.readTVarIO env.var
  SaveSetting setting -> do
    STM.atomically $ writeTVar env.var setting
    LBS.writeFile env.path (encode setting)

-- | Load user preference from file, falling back to default.
loadSettingFromFile :: (FileSystem :> es) => FilePath -> Eff es UserPreference
loadSettingFromFile path = do
  exists <- doesFileExist path
  if exists
    then fromRight defaultUserPreference . eitherDecode <$> LBS.readFile path
    else pure defaultUserPreference
