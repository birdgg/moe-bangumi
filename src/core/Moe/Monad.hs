module Moe.Monad
  ( MoeEffects,
    MoeM,
    runMoe,
  )
where

import Effectful
import Effectful.Error.Static (Error, runErrorNoCallStack)
import Effectful.Sqlite (SQLite, runSQLiteWithPath)
import Moe.Environment.Config (MoeError)
import Moe.Environment.Env (MoeEnv (..))
import Moe.Model.Bangumi.Interpreter (runBangumiQuerySQLite, runBangumiUpdateSQLite)
import Moe.Model.Bangumi.Query (BangumiQuery)
import Moe.Model.Bangumi.Update (BangumiUpdate)
import Moe.Model.BangumiData.Interpreter (runBangumiDataHttp)
import Moe.Model.BangumiData.Query (BangumiData)

type MoeEffects =
  '[ BangumiQuery,
     BangumiUpdate,
     BangumiData,
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
    & runSQLiteWithPath env.databasePath
    & runErrorNoCallStack
    & runEff
