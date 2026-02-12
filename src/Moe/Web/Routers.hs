module Moe.Web.Routers where

import GHC.Generics (Generic)
import Servant.API
import Servant.Scalar (ScalarUI)

import Moe.Web.API.Routes qualified as API

type ServerRoutes = NamedRoutes Routes

data Routes mode = Routes
  { api :: mode :- API.Routes,
    doc :: mode :- ScalarUI "docs" "openapi.json",
    spa :: mode :- Raw
  }
  deriving stock (Generic)
