module Moe.Adapter.File.Setting
  ( runSettingFile,
  )
where

import Data.Aeson (eitherDecodeFileStrict, encodeFile)
import Effectful
import Effectful.Dispatch.Dynamic (interpret)
import Moe.Domain.Setting.Types (defaultUserPreference)
import Moe.Effect.Setting (Setting (..))
import System.Directory (doesFileExist)

runSettingFile ::
  (IOE :> es) =>
  FilePath ->
  Eff (Setting : es) a ->
  Eff es a
runSettingFile settingPath = interpret $ \_ -> \case
  GetSetting -> liftIO $ do
    exists <- doesFileExist settingPath
    if exists
      then fromRight defaultUserPreference <$> eitherDecodeFileStrict settingPath
      else pure defaultUserPreference
  SaveSetting setting -> liftIO $ encodeFile settingPath setting
