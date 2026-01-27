module Moe.App.Run
  ( runApp,
  )
where

import Effectful
import Effectful.Error.Static (runErrorNoCallStack)
import Effectful.Sqlite (runSQLiteWithPath)
import Moe.App.Env (AppEnv (..))
import Moe.App.Error (AppError)
import Moe.App.Monad (App)
import Moe.Infra.Effect.Bangumi (runBangumiSQLite)

runApp :: AppEnv -> App a -> IO (Either AppError a)
runApp env app =
  app
    & runBangumiSQLite
    & runSQLiteWithPath env.databasePath
    & runErrorNoCallStack
    & runEff
