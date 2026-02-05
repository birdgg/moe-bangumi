module Moe.App.Job.Types
  ( BaseJobEffects,
    MetadataJobEffects,
    RssJobEffects,
    SubscriptionJobEffects,
    RenameJobEffects,
    JobAction,
  )
where

import Effectful
import Effectful.Concurrent (Concurrent)
import Effectful.Error.Static (Error)
import Effectful.FileSystem (FileSystem)
import Effectful.Log (Log)
import Effectful.Sqlite (Sqlite)
import Moe.Error (MoeError)
import Moe.Infrastructure.BangumiData.Effect (BangumiData)
import Moe.Infrastructure.Download.Effect (Download)
import Moe.Infrastructure.Metadata.Effect (Metadata)
import Moe.Infrastructure.Notification.Effect (Notification)
import Moe.Infrastructure.Rss.Effect (Rss)
import Moe.Infrastructure.Setting.Effect (Setting)

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
     Error MoeError,
     Setting,
     FileSystem,
     Log,
     Sqlite,
     Concurrent,
     IOE
   ]

type RssJobEffects =
  '[ Rss,
     Setting,
     Error MoeError,
     FileSystem,
     Log,
     Sqlite,
     Concurrent,
     IOE
   ]

-- | Effect stack for subscription job (RSS + Download)
type SubscriptionJobEffects =
  '[ Download,
     Rss,
     Error MoeError,
     Setting,
     FileSystem,
     Log,
     Sqlite,
     Concurrent,
     IOE
   ]

-- | Effect stack for rename job (Download + Notification)
type RenameJobEffects =
  '[ Notification,
     Download,
     Error MoeError,
     Setting,
     FileSystem,
     Log,
     Sqlite,
     Concurrent,
     IOE
   ]

type JobAction es a = Eff es a
