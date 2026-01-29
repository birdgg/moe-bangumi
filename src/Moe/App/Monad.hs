module Moe.App.Monad
  ( MoeEffects,
    MoeM,
    runMoe,
  )
where

import Effectful
import Effectful.Error.Static (Error, runErrorNoCallStack)
import Effectful.Log (Log)
import Effectful.Sqlite (SQLite, runSQLiteWithPath)
import Moe.Adapter.Database.Bangumi (runBangumiQuerySQLite, runBangumiUpdateSQLite)
import Moe.Adapter.File.Setting (runSettingFile)
import Moe.Adapter.Http.BangumiData (runBangumiDataHttp)
import Moe.Adapter.Http.Metadata (runMetadataHttp)
import Moe.App.Env (MoeEnv (..), getDatabasePath, getSettingPath)
import Moe.App.Error (MoeError)
import Moe.App.Logging (LogConfig (..), makeLogger, runLog)
import Moe.Effect.Bangumi (BangumiQuery, BangumiUpdate)
import Moe.Effect.BangumiData (BangumiData)
import Moe.Effect.Setting (Setting)
import Moe.Effect.Metadata (Metadata)

type MoeEffects =
  '[ BangumiQuery,
     BangumiUpdate,
     BangumiData,
     Metadata,
     Setting,
     Log,
     SQLite,
     Error MoeError,
     IOE
   ]

type MoeM a = Eff MoeEffects a

runMoe :: MoeEnv -> MoeM a -> IO (Either MoeError a)
runMoe env app =
  runEff
    . runErrorNoCallStack
    . runSQLiteWithPath (getDatabasePath env)
    $ makeLogger env.logConfig.destination
    $ \logger ->
      app
        & runBangumiQuerySQLite
        & runBangumiUpdateSQLite
        & runBangumiDataHttp
        & runMetadataHttp env
        & runSettingFile (getSettingPath env)
        & runLog "moe-bangumi" logger env.logConfig.logLevel
