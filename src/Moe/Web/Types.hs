module Moe.Web.Types
  ( MoeEff,
    MoeEffects,
  )
where

import Effectful
import Effectful.Concurrent (Concurrent)
import Effectful.Error.Static (Error)
import Effectful.Log (Log)
import Effectful.Reader.Static qualified as Reader
import Effectful.Sqlite (Sqlite)
import Moe.App.Env (MoeEnv)
import Moe.App.Error (MoeError)
import Moe.Effect.Metadata (Metadata)
import Moe.Effect.Setting (Setting)
import Servant (ServerError)

type MoeEff = Eff MoeEffects

type MoeEffects =
  '[ Metadata,
     Setting,
     Error MoeError,
     Error ServerError,
     Log,
     Sqlite,
     Concurrent,
     Reader.Reader MoeEnv,
     IOE
   ]
