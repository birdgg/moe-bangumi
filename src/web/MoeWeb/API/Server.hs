module MoeWeb.API.Server where

import MoeWeb.API.Routes qualified as API
import MoeWeb.Types
import Servant

apiServer :: ServerT API.Routes MoeEff
apiServer = pure "ok"
