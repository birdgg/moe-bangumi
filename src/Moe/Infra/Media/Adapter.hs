-- | Emby interpreter for Media effect.
module Moe.Infra.Media.Adapter
  ( -- * Interpreter
    runMediaEmby,

    -- * Re-exports
    module Moe.Infra.Media.Effect,
  )
where

import Data.Text.Display (display)
import Effectful ((:>))
import Effectful.Dispatch.Dynamic (interpret)
import Moe.Domain.Setting (EmbyConfig (..), UserPreference (..))
import Moe.Error (AppError (..))
import Moe.Infra.Media.Client qualified as Client
import Moe.Infra.Media.Effect
import Moe.Infra.Setting.Effect (Setting, getSetting)
import Moe.Prelude
import Network.HTTP.Client (Manager)

-- | Run Media effect using Emby, reading config from Setting.
runMediaEmby ::
  (Setting :> es, Error AppError :> es, IOE :> es) =>
  Manager ->
  Eff (Media : es) a ->
  Eff es a
runMediaEmby manager action = do
  cfg <- (.emby) <$> getSetting
  interpret (\_ -> handleMedia manager cfg) action

-- | Validate that required fields are non-empty.
validateConfig :: (Error AppError :> es) => EmbyConfig -> Eff es ()
validateConfig cfg =
  whenJust (fmtErrors "Emby" $ validateField "url" cfg.url *> validateField "apiKey" cfg.apiKey) $
    throwError . MediaError . MediaConfigError

-- | Handle a single Media operation.
handleMedia ::
  (Error AppError :> es, IOE :> es) =>
  Manager ->
  EmbyConfig ->
  Media (Eff localEs) a ->
  Eff es a
handleMedia manager cfg = \case
  TestMediaConnection url apiKey -> do
    result <- liftIO $ Client.getSystemInfo manager url apiKey
    pure $ first display result
  GetLibraries -> do
    validateConfig cfg
    result <- liftIO $ Client.getVirtualFolders manager cfg.url cfg.apiKey
    liftEitherWith MediaError result
  GetLibraryItems libraryId -> do
    validateConfig cfg
    userId <- liftIO (Client.getUsers manager cfg.url cfg.apiKey) >>= liftEitherWith MediaError
    result <- liftIO $ Client.getItems manager cfg.url cfg.apiKey userId libraryId
    liftEitherWith MediaError result
