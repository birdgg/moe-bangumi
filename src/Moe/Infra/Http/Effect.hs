-- | Generic Http effect for HTTP manager access and Servant client execution.
module Moe.Infra.Http.Effect
  ( -- * Effect
    Http (..),
    getHttpManager,
    runServantClient,

    -- * Environment
    HttpEnv,
    initHttpEnv,

    -- * Interpreter
    runHttp,
  )
where

import Effectful.Internal.Monad (unsafeEff_)
import Effectful.TH (makeEffect)
import Moe.Prelude
import Network.HTTP.Client (Manager)
import Servant.Client (BaseUrl, ClientEnv, ClientError, ClientM)
import Servant.Client qualified as Servant

data Http :: Effect where
  GetHttpManager :: Http m Manager
  RunServantClient :: BaseUrl -> ClientM a -> Http m (Either ClientError a)

type instance DispatchOf Http = 'Dynamic

makeEffect ''Http

-- | Opaque environment holding the shared HTTP manager.
newtype HttpEnv = HttpEnv Manager

-- | Create an HttpEnv from a Manager.
initHttpEnv :: Manager -> HttpEnv
initHttpEnv = HttpEnv

-- | Run the Http effect with the given environment.
runHttp :: HttpEnv -> Eff (Http : es) a -> Eff es a
runHttp (HttpEnv mgr) = interpret $ \_ -> \case
  GetHttpManager -> pure mgr
  RunServantClient baseUrl clientAction -> do
    let env = mkClientEnv mgr baseUrl
    unsafeEff_ $ Servant.runClientM clientAction env

mkClientEnv :: Manager -> BaseUrl -> ClientEnv
mkClientEnv = Servant.mkClientEnv
