module Moe.Web.Types
  ( ServerEff,
    ServerEffects,
  )
where

import Effectful
import Effectful.Concurrent (Concurrent)
import Effectful.Error.Static (Error)
import Effectful.FileSystem (FileSystem)
import Effectful.Log (Log)
import Effectful.Reader.Static (Reader)
import Effectful.Sqlite (Sqlite)
import Moe.App.Env (MoeEnv)
import Moe.Error (AppError)
import Moe.Infra.Downloader.Effect (Downloader)
import Moe.Infra.Media.Effect (Media)
import Moe.Infra.Metadata.Effect (Metadata)
import Moe.Infra.Rss.Effect (Rss)
import Moe.Infra.Setting.Effect (Setting, SettingWriter)

type ServerEff = Eff ServerEffects

type ServerEffects =
  '[ Media,
     Downloader,
     Rss,
     Metadata,
     Setting,
     SettingWriter,
     Sqlite,
     Error AppError,
     Log,
     Reader MoeEnv,
     Concurrent,
     FileSystem,
     IOE
   ]
