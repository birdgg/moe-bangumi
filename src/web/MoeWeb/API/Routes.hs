module MoeWeb.API.Routes where

import Servant

type Routes = "api" :> "health" :> Get '[JSON] Text
