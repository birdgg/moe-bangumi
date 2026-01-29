module Moe.App.Monad
  ( MoeEffects,
    MoeM,
    runMoe,
  )
where

import Effectful
import Effectful.Error.Static (Error, runErrorNoCallStack)
import Effectful.Sqlite (SQLite, runSQLiteWithPath)
import Moe.Adapter.Database.Bangumi (runBangumiQuerySQLite, runBangumiUpdateSQLite)
import Moe.Adapter.Http.BangumiData.Interpreter (runBangumiDataHttp)
import Moe.Adapter.Http.Metadata.Interpreter (runMetadataHttp)
import Moe.App.Env (MoeEnv (..))
import Moe.App.Error (MoeError)
import Moe.Effect.Bangumi (BangumiQuery, BangumiUpdate)
import Moe.Effect.BangumiData (BangumiData)
import Moe.Effect.Metadata (Metadata)

type MoeEffects =
  '[ BangumiQuery,
     BangumiUpdate,
     BangumiData,
     Metadata,
     SQLite,
     Error MoeError,
     IOE
   ]

type MoeM a = Eff MoeEffects a

runMoe :: MoeEnv -> MoeM a -> IO (Either MoeError a)
runMoe env app =
  app
    & runBangumiQuerySQLite
    & runBangumiUpdateSQLite
    & runBangumiDataHttp
    & runMetadataHttp env.metadataConfig
    & runSQLiteWithPath env.databasePath
    & runErrorNoCallStack
    & runEff
