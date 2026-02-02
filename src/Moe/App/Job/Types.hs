module Moe.App.Job.Types
  ( BaseJobEffects,
    MetadataJobEffects,
    RssJobEffects,
    JobAction,
  )
where

import Data.Text (Text)
import Effectful
import Effectful.Concurrent (Concurrent)
import Effectful.Error.Static (Error)
import Effectful.Log (Log)
import Effectful.Sqlite (Sqlite)
import Moe.Effect.BangumiData (BangumiData)
import Moe.Effect.Metadata (Metadata)
import Moe.Effect.Rss (Rss)
import Moe.Effect.Setting (Setting)
import Moe.Error (MoeError)

type BaseJobEffects =
  '[ Error MoeError,
     Log,
     Sqlite,
     Concurrent,
     IOE
   ]

type MetadataJobEffects =
  '[ Metadata,
     BangumiData,
     Error Text,
     Error MoeError,
     Setting,
     Log,
     Sqlite,
     Concurrent,
     IOE
   ]

type RssJobEffects =
  '[ Rss,
     Error MoeError,
     Log,
     Sqlite,
     Concurrent,
     IOE
   ]

type JobAction es a = Eff es a
