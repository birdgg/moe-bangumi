module Moe.Web.API.Routes where

import Servant

type Routes = "api" :> NamedRoutes Routes'

data Routes' mode = Routes'
  { health :: mode :- "health" :> Get '[JSON] Text
  }
  deriving stock (Generic)
