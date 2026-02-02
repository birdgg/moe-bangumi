module Moe.App.RssSync.Pipeline
  ( runPipeline,
    module Moe.App.RssSync.Types,
  )
where

import Data.Text (Text)
import Data.Text qualified as T
import Effectful
import Effectful.Concurrent (Concurrent)
import Effectful.Log (Log)
import Effectful.Log qualified as Log
import Effectful.Sqlite (Sqlite)
import Moe.App.RssSync.Download (downloadAll)
import Moe.App.RssSync.Fetch (fetchAll)
import Moe.App.RssSync.Filter (filterFetchResults)
import Moe.App.RssSync.Types
import Moe.Domain.Setting.Types (UserPreference (..))
import Moe.Infrastructure.Rss.Effect (Rss)
import Moe.Infrastructure.Setting.Effect (Setting, getSetting)
import Network.HTTP.Client (Manager)

runPipeline ::
  (Rss :> es, Setting :> es, Sqlite :> es, Concurrent :> es, Log :> es, IOE :> es) =>
  Manager ->
  Eff es PipelineResult
runPipeline manager = do
  Log.logInfo_ "Starting RSS sync pipeline..."

  fetchResults <- fetchAll manager
  let fetchedCount = sum $ map (length . (.items)) fetchResults

  pref <- getSetting
  let filterConfig = pref.filter
  let filteredItems = filterFetchResults filterConfig fetchResults
  let filteredCount = length filteredItems

  downloadTasks <- downloadAll filteredItems
  let downloadedCount = length downloadTasks

  Log.logInfo_ $ "Pipeline complete: fetched=" <> showT fetchedCount
    <> " filtered=" <> showT filteredCount
    <> " downloaded=" <> showT downloadedCount

  pure PipelineResult
    { fetchedCount = fetchedCount,
      filteredCount = filteredCount,
      downloadedCount = downloadedCount
    }
  where
    showT :: (Show a) => a -> Text
    showT = T.pack . show
