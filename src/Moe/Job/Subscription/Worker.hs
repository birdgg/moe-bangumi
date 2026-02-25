-- | RSS worker thread: combines periodic polling with queue-driven processing.
-- Replaces cron-based subscriptionJob with a long-running thread that both
-- polls getSubscriptionContexts on a timer and processes handler-pushed items.
module Moe.Job.Subscription.Worker
  ( rssWorkerThread,
  )
where

import Data.Aeson (object, (.=))
import Data.Text.Display (Display, display)
import Effectful
import Effectful.Concurrent (threadDelay)
import Effectful.Concurrent.STM qualified as STM
import Effectful.Error.Static (runErrorWith)
import Effectful.Log (Logger)
import Effectful.Log qualified as Log
import Moe.App.Env (MoeEnv (..))
import Moe.Domain.Bangumi (Bangumi (..))
import Moe.Domain.Shared.Entity (Entity (..))
import Moe.Infra.Database.Types (DatabaseExecError)
import Moe.Infra.Downloader.Adapter (runDownloaderQBittorrent)
import Moe.Infra.Downloader.Types (DownloaderClientError)
import Moe.Infra.Rss.Effect (runRss)
import Moe.Infra.Rss.Types (RssFetchError)
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
      rssWorkerLoop env

-- | Main worker loop: poll on timer, process queue items in between.
rssWorkerLoop ::
  (Setting :> es, Sqlite :> es, Error DatabaseExecError :> es, Concurrent :> es, Log :> es, IOE :> es) =>
  MoeEnv ->
  Eff es ()
rssWorkerLoop env = do
  Log.logInfo_ "RSS worker started"
  -- Initial poll on startup
  pollAndProcessAll env env.rssQueue
  -- Main loop
  void $ infinitely $ do
    timerVar <- STM.registerDelay thirtyMinutes
    -- Inner loop: process queue items until timer fires
    fix $ \go -> do
      action <-
        STM.atomically $
          (QueueItem <$> STM.readTQueue env.rssQueue)
            `STM.orElse` (TimerExpired <$ (STM.readTVar timerVar >>= STM.check))
      case action of
        QueueItem ctx -> do
          processSingleFeed env ctx
          go
        TimerExpired -> pass
    -- Timer expired: full poll
    pollAndProcessAll env env.rssQueue

-- -------------------------------------------------------------------
-- Internal
-- -------------------------------------------------------------------

data WorkerAction
  = QueueItem RssContext
  | TimerExpired

-- | Process a single feed, introducing Rss/Downloader interpreters per-feed
-- for natural error isolation.
processSingleFeed ::
  (Setting :> es, Sqlite :> es, Error DatabaseExecError :> es, Concurrent :> es, Log :> es, IOE :> es) =>
  MoeEnv ->
  RssContext ->
  Eff es ()
processSingleFeed env ctx =
  runErrorWith @RssFetchError (logFeedError ctx) $
    runRss env.httpManager $
      runErrorWith @DownloaderClientError (logFeedError ctx) $
        runDownloaderQBittorrent env.downloaderEnv env.httpManager $
          processFeed ctx

-- | Poll all subscription contexts and process them, then drain the queue.
pollAndProcessAll ::
  (Setting :> es, Sqlite :> es, Error DatabaseExecError :> es, Concurrent :> es, Log :> es, IOE :> es) =>
  MoeEnv ->
  STM.TQueue RssContext ->
  Eff es ()
pollAndProcessAll env queue = do
  contexts <- getSubscriptionContexts
  for_ contexts $ \ctx -> do
    processSingleFeed env ctx
    threadDelay 500_000
  -- Drain queue to avoid processing duplicates after full poll
  void $ STM.atomically $ STM.flushTQueue queue

-- | Log an error with bangumi title and RSS URL context.
logFeedError :: (Display e, Log :> es) => RssContext -> a -> e -> Eff es ()
logFeedError ctx _ err =
  Log.logAttention "RSS processing failed" $
    object
      [ "bangumi" .= ctx.bangumi.entityVal.titleChs,
        "rss" .= ctx.rssUrl,
        "error" .= display err
      ]

thirtyMinutes :: Int
thirtyMinutes = 30 * 60 * 1_000_000
