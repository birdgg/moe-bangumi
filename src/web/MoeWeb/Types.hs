module MoeWeb.Types
  ( RouteEffects,
    MoeEff,
  )
where

import Effectful
import Effectful.Error.Static (Error)
import Servant (ServerError)

type MoeEff = Eff RouteEffects

type RouteEffects =
  '[ Error ServerError,
     IOE
   ]
