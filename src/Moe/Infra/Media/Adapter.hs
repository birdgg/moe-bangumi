-- | Plex interpreter for Media effect.
module Moe.Infra.Media.Adapter
  ( -- * Interpreter
    runMediaDynamic,

    -- * Re-exports
    module Moe.Infra.Media.Effect,
  )
where

import Effectful.Log qualified as Log
import Moe.Domain.Setting (MediaConfig (..), UserPreference (..))
import Moe.Infra.Media.Client qualified as Client
import Moe.Infra.Media.Effect
import Moe.Infra.Setting.Effect (Setting, getSetting)
import Moe.Prelude
import Network.HTTP.Client (Manager)

-- | Dynamic Media interpreter that reads config from Setting on each call.
-- Silently skips when media config is not set.
-- Logs errors but does not propagate them.
runMediaDynamic ::
  (Setting :> es, Log :> es, IOE :> es) =>
  Manager ->
  Eff (Media : es) a ->
  Eff es a
runMediaDynamic manager = interpret $ \_ -> \case
  RefreshLibrary -> do
    pref <- getSetting
    let cfg = pref.media
    if cfg.plexUrl == "" || cfg.plexToken == ""
      then pass
      else do
        result <- Client.refreshPlexLibrary manager cfg.plexUrl cfg.plexToken
        case result of
          Left err -> Log.logAttention_ err
          Right () -> Log.logInfo_ "Plex library refresh triggered"
