-- | Subscription rename strategy.
--
-- Handles single-file torrents added via RSS subscription.
-- The torrent name is already formatted by the subscription job,
-- so only needs extension appended and then started for download.
module Moe.Job.Rename.Strategy.Subscription
  ( renameSubscription,
  )
where

import Effectful
import Effectful.Log qualified as Log
import Effectful.Sqlite (transact)
import Moe.Domain.Bangumi (Bangumi (..))
import Moe.Domain.Episode (Episode (..))
import Moe.Domain.Shared.Entity (Entity (..))
import Moe.Infra.Database.Bangumi qualified as BangumiDB
import Moe.Infra.Database.Episode qualified as EpisodeDB
import Moe.Infra.Downloader.Effect
import Moe.Infra.Notification.Effect (Notification)
import Moe.Job.Rename.Util (notifySafe)
import Moe.Prelude
import System.FilePath (takeExtension)

-- | Rename a subscription torrent and start download.
renameSubscription ::
  (Downloader :> es, Notification :> es, Sqlite :> es, Concurrent :> es, Log :> es, IOE :> es) =>
  TorrentInfo ->
  Text ->
  Eff es ()
renameSubscription torrent hash = do
  files <- getTorrentFiles hash
  case files of
    [file] -> do
      let ext = toText $ takeExtension (toString file.name)
          newFileName = torrent.name <> ext
      renameTorrentFile hash file.name newFileName
      Log.logInfo_ $ "Rename " <> file.name <> " to " <> newFileName
      removeTagsFromTorrents [hash] [renameTag]
      startTorrents [hash]
      (title, posterUrl) <- transact $ do
        episodes <- EpisodeDB.getEpisodesByInfoHashes [hash]
        case episodes of
          (ep : _) -> do
            let episode = ep.entityVal :: Episode
                epNum = toText episode.episodeNumber
            mBangumi <- BangumiDB.getBangumi episode.bangumiId
            pure $ case mBangumi of
              Just (b :: Entity Bangumi) -> (b.entityVal.titleChs <> " 第" <> epNum <> "集", b.entityVal.posterUrl)
              Nothing -> (torrent.name, Nothing)
          [] -> pure (torrent.name, Nothing)
      notifySafe title posterUrl
    _ ->
      Log.logAttention_ $ "rename: skipping multi-file torrent " <> torrent.name
