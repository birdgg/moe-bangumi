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
import Moe.Infrastructure.BangumiData.Effect (BangumiData)
import Moe.Infrastructure.Metadata.Effect (Metadata)
import Moe.Infrastructure.Rss.Effect (Rss)
import Moe.Infrastructure.Setting.Effect (Setting)
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
     Setting,
     Error MoeError,
     Log,
     Sqlite,
     Concurrent,
     IOE
   ]

type JobAction es a = Eff es a
