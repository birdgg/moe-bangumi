module Moe.Web.API.Server where

import Servant

import Moe.Web.API.Routes qualified as API
import Moe.Web.Types

apiServer :: ServerT API.Routes MoeEff
apiServer =
  API.Routes'
    { health = pure "ok"
    }
