module Moe.Web.Types
  ( ServerEff,
    ServerEffects,
  )
where

import Moe.App.Env (MoeEnv)
import Moe.Prelude
import Moe.Infra.Database.Types (DatabaseExecError)
import Moe.Infra.Downloader.Effect (Downloader)
import Moe.Infra.Downloader.Types (DownloaderError)
import Moe.Infra.Metadata.Effect (Metadata)
import Moe.Infra.Metadata.Types (MetadataFetchError)
import Moe.Infra.Rss.Effect (Rss)
import Moe.Infra.Rss.Types (RssFetchError)
import Moe.Infra.Setting.Effect (Setting, SettingWriter)
import Moe.Infra.Update.Effect (Update)
import Moe.Infra.Update.Types (UpdateError)
import Moe.Web.Error (WebError)

type ServerEff = Eff ServerEffects

type ServerEffects =
  '[ Update,
     Error UpdateError,
     Downloader,
     Error DownloaderError,
     Rss,
     Error RssFetchError,
     Metadata,
     Error MetadataFetchError,
     Setting,
     SettingWriter,
     Sqlite,
     Error DatabaseExecError,
     Error WebError,
     Log,
     Reader MoeEnv,
     Concurrent,
     FileSystem,
     Environment,
     Time,
     IOE
   ]
