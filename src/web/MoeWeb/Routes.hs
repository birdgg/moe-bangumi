module MoeWeb.Routes where

import Data.OpenApi (OpenApi)
import MoeWeb.API.Routes qualified as API
import MoeWeb.Common.Scalar (HTML, RawHtml)
import Servant.API

type ServerRoutes = NamedRoutes Routes

data Routes mode = Routes
  { api :: mode :- API.Routes,
    doc ::
      mode
        :- "docs"
          :> NamedRoutes DocRoutes,
    assets :: mode :- Raw
  }
  deriving stock (Generic)

data DocRoutes mode = DocRoutes
  { docUI ::
      mode
        :- Get '[HTML] RawHtml,
    openApiSpec ::
      mode
        :- "openapi.json"
          :> Get '[JSON] OpenApi
  }
  deriving stock (Generic)
