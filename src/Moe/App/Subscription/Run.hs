module Moe.App.Subscription.Run
  ( runSubscription,
    module Moe.App.Subscription.Types,
  )
where

import Data.Aeson (object, (.=))
import Effectful
import Effectful.Concurrent (Concurrent)
import Effectful.Concurrent.Async (forConcurrently_)
import Effectful.Concurrent.QSem (newQSem, signalQSem, waitQSem)
import Effectful.Exception (bracket_, throwIO, try)
import Effectful.Error.Static (Error, runErrorNoCallStack)
import Effectful.Log (Log)
import Effectful.Log qualified as Log
import Effectful.Sqlite (Sqlite, transact)
import Moe.App.Subscription.Filter (filterItems)
import Moe.App.Subscription.Types
import Moe.App.Subscription.Washing (WashingResult (..), buildEpisodeMap, parseRawItem, processWashing)
import Moe.Domain.Bangumi.Episode (Episode (..))
import Moe.Domain.Bangumi.Types (BangumiF (..))
import Moe.Domain.Setting.Types (UserPreference (..))
import Moe.Error (MoeError)
import Moe.Infrastructure.Database.Episode qualified as EpisodeDB
import Moe.Infrastructure.Database.Tracking qualified as TrackingDB
import Moe.Infrastructure.Download.Effect
import Moe.Infrastructure.Rss.Effect (Rss, RssError, fetchRss)
import Moe.Infrastructure.Setting.Effect (Setting, getSetting)
import Moe.Prelude

runSubscription ::
  (Rss :> es, Download :> es, Error MoeError :> es, Setting :> es, Sqlite :> es, Concurrent :> es, Log :> es, IOE :> es) =>
  Eff es ()
runSubscription = do
  Log.logInfo_ "Starting RSS subscription"

  contexts <- toRssContexts <$> transact TrackingDB.listEnabledRssTrackingWithBangumi
  pref <- getSetting
  sem <- newQSem 5
  forConcurrently_ contexts $ \ctx -> do
    result <- try @SomeException $
      bracket_
        (waitQSem sem)
        (signalQSem sem)
        (runSingleRss pref ctx)
    case result of
      Left ex ->
        Log.logAttention "RSS processing failed" $
          object
            [ "bangumi" .= ctx.bangumi.titleChs,
              "rss" .= ctx.rssUrl,
              "error" .= displayException ex
            ]
      Right () -> pass

-- | Process a single RSS source: fetch -> filter -> wash -> download -> save
runSingleRss ::
  (Rss :> es, Download :> es, Sqlite :> es, Concurrent :> es, Log :> es, IOE :> es) =>
  UserPreference ->
  RssContext ->
  Eff es ()
runSingleRss pref ctx = do
  items <- runErrorNoCallStack @RssError (fetchRss ctx.rssUrl) >>= either throwIO pure
  let filtered = filterItems pref.filter ctx items
  washingResults <- transact $ do
    let parsedEpisodes = mapMaybe (parseRawItem ctx.bangumi) filtered
        bid = ctx.bangumi.id
    episodes <- EpisodeDB.listEpisodesByBangumi bid
    let episodeMap = buildEpisodeMap episodes
    pure $ map (processWashing episodeMap pref.washing) parsedEpisodes

  let (newEpisodes, upgradeEpisodes) = partitionResults washingResults
  processNewEpisodes newEpisodes
  processUpgrades upgradeEpisodes

partitionResults :: [WashingResult] -> ([Episode], [(Episode, Episode)])
partitionResults = foldr go ([], [])
  where
    go SkipEpisode acc = acc
    go (NewEpisode ep) (news, ups) = (ep : news, ups)
    go (UpgradeEpisode newEp oldEp) (news, ups) = (news, (newEp, oldEp) : ups)

-- | Process new episodes: add torrent and save to database
processNewEpisodes ::
  (Download :> es, Sqlite :> es, Concurrent :> es, Log :> es, IOE :> es) =>
  [Episode] ->
  Eff es ()
processNewEpisodes [] = pass
processNewEpisodes episodes = do
  forM_ episodes $ \ep -> do
    let tags = MoeTagList [Subscription]
    addTorrent ep.torrentUrl Nothing (Just tags)

  transact $ mapM_ EpisodeDB.upsertEpisode episodes

-- | Process upgrades: delete old torrents and add new ones.
processUpgrades ::
  (Download :> es, Sqlite :> es, Concurrent :> es, Log :> es, IOE :> es) =>
  [(Episode, Episode)] ->
  Eff es ()
processUpgrades [] = pass
processUpgrades tasks = do
  let oldHashes = map (\(_, oldEp) -> oldEp.infoHash) tasks
  deleteTorrents oldHashes True

  forM_ tasks $ \(newEp, _) -> do
    let tags = MoeTagList [Subscription]
    addTorrent newEp.torrentUrl Nothing (Just tags)

  transact $ mapM_ (\(newEp, _) -> EpisodeDB.upsertEpisode newEp) tasks
