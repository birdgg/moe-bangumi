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
import Moe.App.Subscription.Washing (buildEpisodeMap, parseRawItem, processWashing)
import Moe.Domain.Bangumi.Episode (Episode (..))
import Moe.Domain.Bangumi.File.Naming (generateBaseName, generatePath)
import Moe.Domain.Bangumi.File.Types (toBangumiFile)
import Moe.Domain.Bangumi.Types (Bangumi, BangumiF (..))
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

runSingleRss ::
  (Rss :> es, Download :> es, Sqlite :> es, Concurrent :> es, Log :> es, IOE :> es) =>
  UserPreference ->
  RssContext ->
  Eff es ()
runSingleRss pref ctx = do
  items <- runErrorNoCallStack @RssError (fetchRss ctx.rssUrl) >>= either throwIO pure

  let filtered = filterItems pref.filter ctx items
      parsedEpisodes = mapMaybe (parseRawItem ctx.bangumi) filtered

  episodes <- transact $ EpisodeDB.listEpisodesByBangumi ctx.bangumi.id

  let episodeMap = buildEpisodeMap episodes
      (toAdd, toDelete) = processWashing episodeMap pref.washing parsedEpisodes

  deleteReplacedTorrents toDelete
  downloadAndSaveEpisodes ctx.bangumi toAdd

-- | Download torrents and save episodes to database
downloadAndSaveEpisodes ::
  (Download :> es, Sqlite :> es, Concurrent :> es, Log :> es, IOE :> es) =>
  Bangumi ->
  [Episode] ->
  Eff es ()
downloadAndSaveEpisodes _ [] = pass
downloadAndSaveEpisodes bangumi episodes = do
  forM_ episodes $ addSubscriptionTorrent bangumi
  transact $ mapM_ EpisodeDB.upsertEpisode episodes

-- | Delete old torrents that were replaced by upgrades
deleteReplacedTorrents ::
  (Download :> es, Log :> es, IOE :> es) =>
  [Episode] ->
  Eff es ()
deleteReplacedTorrents [] = pass
deleteReplacedTorrents episodes = do
  let hashes = map (.infoHash) episodes
  deleteTorrents hashes True

-- | Add torrent for subscription download with media-server-compliant file structure
addSubscriptionTorrent :: (Download :> es) => Bangumi -> Episode -> Eff es ()
addSubscriptionTorrent bangumi ep = do
  let file = toBangumiFile bangumi ep
      params =
        AddTorrentParams
          { url = ep.torrentUrl,
            savePath = Just $ toText $ generatePath file,
            rename = Just $ toText $ generateBaseName file,
            tags = Just [subscriptionTag]
          }
  addTorrent params
