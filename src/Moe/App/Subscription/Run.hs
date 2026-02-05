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
import Moe.Domain.Bangumi.File.Types (BangumiFile (..), BangumiMeta (..), EpisodeType (..), FileType (..), VideoExt (..))
import Moe.Domain.Bangumi.File.Types qualified as File (BangumiContent (..))
import Moe.Domain.Bangumi.Types (Bangumi, BangumiF (..), SeasonNumber (..), extractYear)
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
  (toAdd, toDelete) <- transact $ do
    let parsedEpisodes = mapMaybe (parseRawItem ctx.bangumi) filtered
        bid = ctx.bangumi.id
    episodes <- EpisodeDB.listEpisodesByBangumi bid
    let episodeMap = buildEpisodeMap episodes
    pure $ processWashing episodeMap pref.washing parsedEpisodes

  processDeletes toDelete
  processDownloads ctx.bangumi toAdd

-- | Download episodes and save to database
processDownloads ::
  (Download :> es, Sqlite :> es, Concurrent :> es, Log :> es, IOE :> es) =>
  Bangumi ->
  [Episode] ->
  Eff es ()
processDownloads _ [] = pass
processDownloads bangumi episodes = do
  forM_ episodes $ addSubscriptionTorrent bangumi
  transact $ mapM_ EpisodeDB.upsertEpisode episodes

-- | Delete old torrents that were replaced by upgrades
processDeletes ::
  (Download :> es, Log :> es, IOE :> es) =>
  [Episode] ->
  Eff es ()
processDeletes [] = pass
processDeletes episodes = do
  let hashes = map (.infoHash) episodes
  deleteTorrents hashes True

addSubscriptionTorrent :: (Download :> es) => Bangumi -> Episode -> Eff es ()
addSubscriptionTorrent bangumi ep = do
  let file = toBangumiFile bangumi ep
      params =
        AddTorrentParams
          { url = ep.torrentUrl,
            savePath = Just $ toText $ generatePath file,
            rename = Just $ toText $ generateBaseName file,
            tags = Just $ MoeTagList [Subscription]
          }
  addTorrent params

toBangumiFile :: Bangumi -> Episode -> BangumiFile
toBangumiFile bangumi ep =
  let meta =
        BangumiMeta
          { name = bangumi.titleChs,
            year = extractYear <$> bangumi.airDate,
            tmdbId = bangumi.tmdbId
          }
      seasonNum = fromMaybe (SeasonNumber 1) bangumi.season
      content = File.Episode (Regular seasonNum ep.episodeNumber)
   in BangumiFile {meta, content, fileType = Video MKV, group = ep.group}
