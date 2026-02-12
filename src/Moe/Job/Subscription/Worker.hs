-- | RSS worker thread: combines periodic polling with queue-driven processing.
-- Replaces cron-based subscriptionJob with a long-running thread that both
-- polls getSubscriptionContexts on a timer and processes handler-pushed items.
module Moe.Job.Subscription.Worker
  ( rssWorkerThread,
  )
where

import Data.Aeson (object, (.=))
import Data.Text.Display (display)
import Effectful
import Effectful.Concurrent (threadDelay)
import Effectful.Concurrent.STM qualified as STM
import Effectful.Error.Static (runErrorWith)
import Effectful.Log (Logger)
import Effectful.Log qualified as Log
import Moe.App.Env (MoeEnv (..))
import Moe.Domain.Bangumi (Bangumi (..))
import Moe.Domain.Shared.Entity (Entity (..))
import Moe.Error (AppError)
import Moe.Infra.Downloader.Adapter (runDownloaderQBittorrent)
import Moe.Infra.Downloader.Effect (Downloader)
import Moe.Infra.Rss.Effect (Rss, runRss)
import Moe.Infra.Setting.Effect (Setting, runSetting)
import Moe.Job.Effect (runBaseEffects)
import Moe.Job.Subscription.Process (getSubscriptionContexts, processFeed)
import Moe.Job.Subscription.Types (RssContext (..))
import Moe.Prelude

-- | Entry point for the RSS worker thread.
rssWorkerThread :: MoeEnv -> Logger -> IO ()
rssWorkerThread env logger =
  runBaseEffects env logger "Subscription" $
    runSetting env.settingEnv $
      runRss env.httpManager $
        runDownloaderQBittorrent env.downloaderEnv env.httpManager $
          rssWorkerLoop env.rssQueue

-- | Main worker loop: poll on timer, process queue items in between.
rssWorkerLoop ::
  (Rss :> es, Downloader :> es, Setting :> es, Sqlite :> es, Concurrent :> es, Log :> es, IOE :> es) =>
  STM.TQueue RssContext ->
  Eff es ()
rssWorkerLoop queue = do
  Log.logInfo_ "RSS worker started"
  -- Initial poll on startup
  pollAndProcessAll queue
  -- Main loop
  void $ infinitely $ do
    timerVar <- STM.registerDelay thirtyMinutes
    -- Inner loop: process queue items until timer fires
    fix $ \go -> do
      action <-
        STM.atomically $
          (QueueItem <$> STM.readTQueue queue)
            `STM.orElse` (TimerExpired <$ (STM.readTVar timerVar >>= STM.check))
      case action of
        QueueItem ctx -> do
          processSingleFeed ctx
          go
        TimerExpired -> pass
    -- Timer expired: full poll
    pollAndProcessAll queue

-- -------------------------------------------------------------------
-- Internal
-- -------------------------------------------------------------------

data WorkerAction
  = QueueItem RssContext
  | TimerExpired

-- | Process a single feed from the queue, logging errors without propagating.
processSingleFeed ::
  (Rss :> es, Downloader :> es, Setting :> es, Sqlite :> es, Concurrent :> es, Log :> es, IOE :> es) =>
  RssContext ->
  Eff es ()
processSingleFeed ctx =
  runErrorWith (logFeedError ctx) $ processFeed ctx

-- | Poll all subscription contexts and process them, then drain the queue.
pollAndProcessAll ::
  (Rss :> es, Downloader :> es, Setting :> es, Sqlite :> es, Concurrent :> es, Log :> es, IOE :> es) =>
  STM.TQueue RssContext ->
  Eff es ()
pollAndProcessAll queue = do
  contexts <- getSubscriptionContexts
  for_ contexts $ \ctx -> do
    runErrorWith (logFeedError ctx) $ processFeed ctx
    threadDelay 500_000
  -- Drain queue to avoid processing duplicates after full poll
  void $ STM.atomically $ STM.flushTQueue queue

-- | Log an error with bangumi title and RSS URL context.
logFeedError :: (Log :> es) => RssContext -> a -> AppError -> Eff es ()
logFeedError ctx _ err =
  Log.logAttention "RSS processing failed" $
    object
      [ "bangumi" .= ctx.bangumi.entityVal.titleChs,
        "rss" .= ctx.rssUrl,
        "error" .= display err
      ]

thirtyMinutes :: Int
thirtyMinutes = 30 * 60 * 1_000_000
