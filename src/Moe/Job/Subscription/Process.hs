-- | Core subscription processing: fetches RSS feeds, filters items,
-- processes washing, and downloads new episodes.
module Moe.Job.Subscription.Process
  ( triggerSingleSubscription,
    processFeed,
    getSubscriptionContexts,
  )
where

import Data.Aeson (object, (.=))
import Data.Time.Calendar.WeekDate (toWeekDate)
import Data.Time.Clock (getCurrentTime, utctDay)
import Effectful
import Effectful.Concurrent.STM qualified as STM
import Effectful.Log qualified as Log
import Effectful.Reader.Static (ask)
import Effectful.Sqlite (transact)
import Moe.App.Env (MoeEnv (..))
import Moe.Domain.Bangumi (Bangumi (..))
import Moe.Domain.Episode (Episode (..))
import Moe.Domain.Setting (UserPreference (..), WashingConfig (..))
import Moe.Domain.Shared.Entity (Entity (..))
import Moe.Domain.Shared.Numbering (applyEpisodeOffset)
import Moe.Infra.Database.Episode qualified as EpisodeDB
import Moe.Infra.Database.Tracking qualified as TrackingDB
import Moe.Infra.Downloader.Effect (Downloader)
import Moe.Infra.Rss.Effect (Rss, fetchRss)
import Moe.Infra.Rss.Types ()
import Moe.Infra.Setting.Effect (Setting, getSetting)
import Moe.Job.Subscription.Download (deleteReplacedTorrents, downloadAndSaveEpisodes)
import Moe.Job.Subscription.Filter (filterItems, selectEpisodes)
import Moe.Job.Subscription.Types
import Moe.Job.Subscription.Washing (buildEpisodeMap, parseRawItem, processWashing)
import Moe.Prelude

-- -------------------------------------------------------------------
-- Public entry points
-- -------------------------------------------------------------------

-- | Queue a single bangumi subscription for processing by the RSS worker.
triggerSingleSubscription ::
  (Concurrent :> es, Log :> es, Reader MoeEnv :> es) =>
  Entity Bangumi -> Text -> Word32 -> Eff es ()
triggerSingleSubscription bangumi rssUrl episodeOffset = do
  env <- ask
  Log.logInfo "Queueing subscription" $ object ["bangumi" .= bangumi.entityVal.titleChs, "rss" .= rssUrl]
  STM.atomically $ STM.writeTQueue env.rssQueue ctx
  where
    ctx = RssContext {bangumi, rssUrl, lastPubdate = Nothing, autoComplete = True, episodeOffset}

-- -------------------------------------------------------------------
-- Internal
-- -------------------------------------------------------------------

-- | Query DB for today's enabled RSS trackings.
getSubscriptionContexts ::
  (Sqlite :> es, Concurrent :> es, Log :> es, IOE :> es) =>
  Eff es [RssContext]
getSubscriptionContexts = do
  today <- liftIO $ utctDay <$> getCurrentTime
  let (_, _, wd) = toWeekDate today
      sqliteWd = wd `mod` 7
  toRssContexts <$> transact (TrackingDB.listEnabledRssTrackingWithBangumiByWeekday sqliteWd)

-- | Process a single RSS feed: fetch, filter, wash, and download.
processFeed ::
  (Rss :> es, Downloader :> es, Setting :> es, Sqlite :> es, Concurrent :> es, Log :> es, IOE :> es) =>
  RssContext ->
  Eff es ()
processFeed ctx = do
  pref <- getSetting
  Log.logInfo_ $ "Fetching RSS: " <> ctx.bangumi.entityVal.titleChs <> " (" <> ctx.rssUrl <> ")"
  items <- fetchRss ctx.rssUrl

  let filtered = filterItems pref.filter ctx items

  let parsedEpisodes = mapMaybe (parseRawItem pref.washing.groupPriority ctx.bangumi) filtered
      selected = selectEpisodes ctx.autoComplete parsedEpisodes
      adjusted = map (\ep -> ep {episodeNumber = applyEpisodeOffset ctx.episodeOffset ep.episodeNumber}) selected

  episodes <- transact $ EpisodeDB.listEpisodesByBangumi ctx.bangumi.entityId

  let episodeMap = buildEpisodeMap episodes
      (toAdd, toDelete) = processWashing episodeMap pref.washing adjusted

  unless (null toAdd) $
    Log.logInfo "New episodes to add" $ object ["episodes" .= map (\ep -> object ["episode" .= ep.episodeNumber, "group" .= ep.group]) toAdd]
  deleteReplacedTorrents toDelete
  downloadAndSaveEpisodes ctx.bangumi toAdd

