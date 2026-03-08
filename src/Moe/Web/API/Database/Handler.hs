module Moe.Web.API.Database.Handler
  ( handleDownloadDatabase,
  )
where

import Moe.App.Env (MoeConfig (..), MoeEnv (..))
import Moe.Prelude
import Moe.Web.Types (ServerEff)
import Servant (Headers, Header, addHeader)
import System.FilePath ((</>))

handleDownloadDatabase :: ServerEff (Headers '[Header "Content-Disposition" Text] ByteString)
handleDownloadDatabase = do
  env <- ask @MoeEnv
  let dbPath = env.config.dataFolder </> "moe-bangumi.db"
  content <- liftIO $ readFileBS dbPath
  pure $ addHeader "attachment; filename=\"moe-bangumi.db\"" content
