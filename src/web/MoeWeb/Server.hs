module MoeWeb.Server
  ( runMoe,
  )
where

import Control.Monad.Except qualified as Except
import Data.OpenApi (OpenApi)
import Data.Text.Display (display)
import Effectful
import Effectful.Error.Static (runErrorNoCallStack)
import MoeWeb.API.Routes qualified as API
import MoeWeb.API.Server qualified as API
import MoeWeb.Common.Scalar (scalarHtml)
import MoeWeb.Routes
import MoeWeb.Types
import Network.Wai qualified as Wai
import Network.Wai.Application.Static
  ( defaultFileServerSettings,
    ss404Handler,
    ssIndices,
    staticApp,
  )
import Network.Wai.Handler.Warp
  ( defaultSettings,
    runSettings,
    setPort,
  )
import Optics.Core
import Servant
  ( Application,
    Context (..),
    Handler,
    ServerError (..),
    Tagged (..),
    serveWithContextT,
  )
import Servant.OpenApi
import Servant.Server.Generic (AsServerT)
import WaiAppStatic.Types (unsafeToPiece)

runMoe :: IO ()
runMoe = do
  let port = 3000
      baseURL = "http://localhost:" <> display port
  putTextLn $ "Starting server on " <> baseURL
  let server = mkServer
      warpSettings = setPort port defaultSettings
  runSettings warpSettings server

mkServer :: Application
mkServer =
  serveWithContextT
    (Proxy @ServerRoutes)
    EmptyContext
    naturalTransform
    moeServer

moeServer :: Routes (AsServerT MoeEff)
moeServer =
  Routes
    { api = API.apiServer,
      doc = docServer,
      assets = spaFileServer "web/dist"
    }

docServer :: DocRoutes (AsServerT MoeEff)
docServer =
  DocRoutes
    { docUI = pure scalarHtml,
      openApiSpec = pure openApiHandler
    }

spaFileServer :: FilePath -> Tagged MoeEff Wai.Application
spaFileServer root = Tagged $ staticApp settings
  where
    settings =
      (defaultFileServerSettings root)
        { ssIndices = [unsafeToPiece "index.html"],
          ss404Handler = Just serveIndexHtml
        }

    serveIndexHtml :: Wai.Application
    serveIndexHtml req =
      staticApp settings indexRequest
      where
        indexRequest =
          req
            { Wai.pathInfo = ["index.html"],
              Wai.rawPathInfo = "/index.html"
            }

naturalTransform :: MoeEff a -> Handler a
naturalTransform app = do
  result <-
    liftIO $
      app
        & runErrorNoCallStack @ServerError
        & runEff
  either Except.throwError pure result

openApiHandler :: OpenApi
openApiHandler =
  toOpenApi (Proxy @API.Routes)
    & #info % #title .~ "Moe Bangumi API"
    & #info % #description ?~ "Moe Bangumi API Documentation"
