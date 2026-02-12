module Moe.Infra.Setting.Effect
  ( Setting (..),
    getSetting,
    SettingWriter (..),
    saveSetting,
    SettingEnv,
    initSettingEnv,
    runSetting,
    runSettingWriter,
  )
where

import Data.Aeson (Result (..), Value (..), decode, eitherDecode, encode, fromJSON, toJSON)
import Data.Aeson.KeyMap qualified as KeyMap
import Effectful
import Effectful.Concurrent.STM qualified as STM
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.FileSystem (doesFileExist)
import Effectful.FileSystem.IO.ByteString.Lazy qualified as LBS
import Effectful.TH (makeEffect)
import Moe.Domain.Setting (UserPreference, defaultUserPreference)
import Moe.Prelude

data Setting :: Effect where
  GetSetting :: Setting m UserPreference

makeEffect ''Setting

data SettingWriter :: Effect where
  SaveSetting :: UserPreference -> SettingWriter m ()

makeEffect ''SettingWriter

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

-- | Run Setting effect (read-only, from TVar).
runSetting ::
  (Concurrent :> es) =>
  SettingEnv ->
  Eff (Setting : es) a ->
  Eff es a
runSetting env = interpret $ \_ -> \case
  GetSetting -> STM.readTVarIO env.var

-- | Run SettingWriter effect (writes to TVar and file).
runSettingWriter ::
  (FileSystem :> es, Concurrent :> es) =>
  SettingEnv ->
  Eff (SettingWriter : es) a ->
  Eff es a
runSettingWriter env = interpret $ \_ -> \case
  SaveSetting setting -> do
    STM.atomically $ writeTVar env.var setting
    LBS.writeFile env.path (encode setting)

-- | Load user preference from file, falling back to default.
-- If the file structure is outdated (missing fields), merges with defaults and writes back.
loadSettingFromFile :: (FileSystem :> es) => FilePath -> Eff es UserPreference
loadSettingFromFile path = do
  exists <- doesFileExist path
  if exists
    then do
      content <- LBS.readFile path
      case eitherDecode content of
        Right pref -> pure pref
        Left _ -> migrateSettingFile path content
    else pure defaultUserPreference

-- | Migrate an outdated setting file by merging with defaults.
migrateSettingFile :: (FileSystem :> es) => FilePath -> LByteString -> Eff es UserPreference
migrateSettingFile path content =
  case decode content of
    Just fileValue ->
      let merged = deepMerge (toJSON defaultUserPreference) fileValue
       in case fromJSON merged of
            Success pref -> do
              LBS.writeFile path (encode pref)
              pure pref
            Error _ -> pure defaultUserPreference
    Nothing -> pure defaultUserPreference

-- | Deep merge two JSON values. Base provides defaults, override takes precedence.
deepMerge :: Value -> Value -> Value
deepMerge (Object base) (Object override) =
  Object (KeyMap.unionWith deepMerge base override)
deepMerge _ override = override
