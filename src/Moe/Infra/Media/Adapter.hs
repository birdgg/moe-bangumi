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
import Moe.Infra.Http.Effect (Http, getHttpManager)
import Moe.Infra.Media.Client qualified as Client
import Moe.Infra.Media.Effect
import Moe.Infra.Setting.Effect (Setting, getSetting)
import Moe.Prelude

-- | Dynamic Media interpreter that reads config from Setting on each call.
-- Silently skips when media config is not set.
-- Logs errors but does not propagate them.
runMediaDynamic ::
  (Http :> es, Setting :> es, Log :> es, IOE :> es) =>
  Eff (Media : es) a ->
  Eff es a
runMediaDynamic = interpret $ \_ -> \case
  RefreshLibrary -> do
    mgr <- getHttpManager
    pref <- getSetting
    let cfg = pref.media
    if cfg.plexUrl == "" || cfg.plexToken == ""
      then pass
      else do
        result <- Client.refreshPlexLibrary mgr cfg.plexUrl cfg.plexToken
        case result of
          Left err -> Log.logAttention_ err
          Right () -> Log.logInfo_ "Plex library refresh triggered"
