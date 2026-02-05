module Moe.App.Effect.Runner
  ( runBaseEffects,
    runMetadataEffects,
    runRssEffects,
    runSubscriptionEffects,
    runRenameEffects,
    runCalendarSyncEffects,
    BaseEffects,
    CalendarSyncEffects,
  )
where

import Effectful
import Effectful.Concurrent (Concurrent, runConcurrent)
import Effectful.Error.Static (Error, runErrorWith)
import Effectful.FileSystem (FileSystem, runFileSystem)
import Effectful.Log (Log, Logger)
import Effectful.Log qualified as Log
import Effectful.Sqlite (Sqlite, SqliteDb (..), runSqlite)
import Moe.App.Env (MoeConfig (..), MoeEnv (..))
import Moe.App.Job.Types (MetadataJobEffects, RenameJobEffects, RssJobEffects, SubscriptionJobEffects)
import Moe.App.Logging (LogConfig (..), runLog)
import Moe.Domain.Scheduler.Types (JobResult (..))
import Moe.Domain.Setting.Types (UserPreference (..))
import Moe.Error (MoeError)
import Moe.Infrastructure.BangumiData.Effect (BangumiData, runBangumiDataHttp)
import Moe.Infrastructure.Download.Adapter (runDownloadQBittorrent)
import Moe.Infrastructure.Notification.Adapter (runNotificationDynamic)
import Moe.Infrastructure.Metadata.Effect (Metadata, runMetadataHttp)
import Moe.Infrastructure.Rss.Effect (runRss)
import Moe.Infrastructure.Setting.Effect (Setting, getSetting, runSetting)
import Moe.Prelude

type BaseEffects =
  '[ Log,
     Sqlite,
     Concurrent,
     IOE
   ]

runBaseEffects ::
  MoeEnv ->
  Logger ->
  Text ->
  Eff BaseEffects a ->
  IO a
runBaseEffects env logger logPrefix =
  runEff
    . withUnliftStrategy (ConcUnlift Ephemeral Unlimited)
    . runConcurrent
    . runSqlite (DbPool env.dbPool)
    . runLog logPrefix logger env.config.logConfig.logLevel

runMetadataEffects ::
  MoeEnv ->
  Logger ->
  Text ->
  Eff MetadataJobEffects JobResult ->
  IO JobResult
runMetadataEffects env logger logPrefix action =
  runBaseEffects env logger logPrefix $
    runFileSystem $
      runSetting env.settingEnv $
        runErrorWith (logMoeError logPrefix) $
          runBangumiDataHttp env.httpManager $
            runMetadataHttp env.httpManager action

runRssEffects ::
  MoeEnv ->
  Logger ->
  Text ->
  Eff RssJobEffects JobResult ->
  IO JobResult
runRssEffects env logger logPrefix action =
  runBaseEffects env logger logPrefix $
    runFileSystem $
      runErrorWith (logMoeError logPrefix) $
        runSetting env.settingEnv $
          runRss env.httpManager action

logMoeError :: (Log :> es) => Text -> a -> MoeError -> Eff es JobResult
logMoeError logPrefix _ err = do
  Log.logAttention_ $ logPrefix <> " error: " <> show err
  pure $ JobFailure $ show err

type CalendarSyncEffects =
  '[ Metadata,
     BangumiData,
     Error MoeError,
     Setting,
     FileSystem,
     Log,
     Sqlite,
     Concurrent,
     IOE
   ]

runCalendarSyncEffects ::
  MoeEnv ->
  Logger ->
  Text ->
  Eff CalendarSyncEffects () ->
  IO ()
runCalendarSyncEffects env logger logPrefix action =
  runBaseEffects env logger logPrefix $
    runFileSystem $
      runSetting env.settingEnv $
        runErrorWith (\_ err -> Log.logAttention_ $ logPrefix <> " error: " <> show err) $
          runBangumiDataHttp env.httpManager $
            runMetadataHttp env.httpManager action

-- | Run rename job effects (Download + Notification).
runRenameEffects ::
  MoeEnv ->
  Logger ->
  Text ->
  Eff RenameJobEffects JobResult ->
  IO JobResult
runRenameEffects env logger logPrefix action =
  runBaseEffects env logger logPrefix $
    runFileSystem $
      runSetting env.settingEnv $
        runErrorWith (logMoeError logPrefix) $ do
        pref <- getSetting
        case pref.downloader of
          Nothing -> do
            Log.logAttention_ $ logPrefix <> " downloader config missing"
            pure $ JobFailure "Downloader config missing"
          Just dlCfg ->
            runDownloadQBittorrent dlCfg $
              runNotificationDynamic env.httpManager action

-- | Run subscription job effects (RSS + Download).
runSubscriptionEffects ::
  MoeEnv ->
  Logger ->
  Text ->
  Eff SubscriptionJobEffects JobResult ->
  IO JobResult
runSubscriptionEffects env logger logPrefix action =
  runBaseEffects env logger logPrefix $
    runFileSystem $
      runSetting env.settingEnv $
        runErrorWith (logMoeError logPrefix) $
        runRss env.httpManager $ do
          pref <- getSetting
          case pref.downloader of
            Nothing -> do
              Log.logAttention_ $ logPrefix <> " downloader config missing"
              pure $ JobFailure "Downloader config missing"
            Just cfg ->
              runDownloadQBittorrent cfg action

