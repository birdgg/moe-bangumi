module Moe.App.Monad
  ( AppEffects,
    App,
  )
where

import Effectful
import Effectful.Error.Static (Error)
import Effectful.Sqlite (SQLite)
import Moe.App.Error (AppError)
import Moe.Effect.Bangumi (Bangumi)

type AppEffects =
  '[ Bangumi,
     SQLite,
     Error AppError,
     IOE
   ]

type App a = Eff AppEffects a
