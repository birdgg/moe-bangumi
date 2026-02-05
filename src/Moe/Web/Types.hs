module Moe.Web.Types
  ( MoeEff,
    MoeEffects,
  )
where

import Effectful
import Effectful.Concurrent (Concurrent)
import Effectful.Error.Static (Error)
import Effectful.FileSystem (FileSystem)
import Effectful.Log (Log)
import Effectful.Reader.Static qualified as Reader
import Effectful.Sqlite (Sqlite)
import Moe.App.Env (MoeEnv)
import Moe.Error (MoeError)
import Moe.Infrastructure.BangumiData.Effect (BangumiData)
import Moe.Infrastructure.Metadata.Effect (Metadata)
import Moe.Infrastructure.Setting.Effect (Setting)
import Servant (ServerError)

type MoeEff = Eff MoeEffects

type MoeEffects =
  '[ Metadata,
     BangumiData,
     Setting,
     FileSystem,
     Error MoeError,
     Error ServerError,
     Log,
     Sqlite,
     Concurrent,
     Reader.Reader MoeEnv,
     IOE
   ]
