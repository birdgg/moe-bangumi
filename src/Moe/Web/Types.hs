module Moe.Web.Types
  ( MoeEff,
    MoeEffects,
  )
where

import Data.Text (Text)
import Effectful
import Effectful.Concurrent (Concurrent)
import Effectful.Error.Static (Error)
import Effectful.Log (Log)
import Effectful.Reader.Static qualified as Reader
import Effectful.Sqlite (Sqlite)
import Moe.App.Env (MoeEnv)
import Moe.Infrastructure.BangumiData.Effect (BangumiData)
import Moe.Infrastructure.Metadata.Effect (Metadata)
import Moe.Infrastructure.Setting.Effect (Setting)
import Moe.Error (MoeError)
import Servant (ServerError)

type MoeEff = Eff MoeEffects

type MoeEffects =
  '[ Metadata,
     BangumiData,
     Setting,
     Error Text,
     Error MoeError,
     Error ServerError,
     Log,
     Sqlite,
     Concurrent,
     Reader.Reader MoeEnv,
     IOE
   ]
