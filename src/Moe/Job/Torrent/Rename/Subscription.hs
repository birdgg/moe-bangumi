-- | Subscription rename strategy.
--
-- Handles subscription torrents (single video with optional subtitles).
-- The torrent name is already formatted by the subscription job,
-- so only needs extension appended and then started for download.
module Moe.Job.Torrent.Rename.Subscription
  ( renameSubscription,
  )
where

import Data.Text qualified as T
import Effectful.Log qualified as Log
import Moe.Domain.Bangumi (Bangumi (..))
import Moe.Domain.Episode (Episode (..))
import Moe.Domain.File (isSubtitleExt, isVideoExt)
import Moe.Domain.Shared.Entity (Entity (..))
import Moe.Domain.Shared.Subtitle (Subtitle, toMediaCode)
import Moe.Infra.Database.Bangumi qualified as BangumiDB
import Moe.Infra.Database.Episode qualified as EpisodeDB
import Moe.Infra.Database.PendingNotification (PendingNotification (..))
import Moe.Infra.Database.PendingNotification qualified as PendingNotificationDB
import Moe.Infra.Downloader.Effect
import Moe.Prelude

-- | Rename a subscription torrent and start download.
renameSubscription ::
  (Downloader :> es, Sqlite :> es, Concurrent :> es, Log :> es, IOE :> es) =>
  TorrentInfo ->
  Text ->
  Eff es ()
renameSubscription torrent hash = do
  files <- getTorrentFiles hash
  let fileExt f = T.toLower $ T.takeWhileEnd (/= '.') f.name
      videos = filter (isVideoExt . fileExt) files
      subs = filter (isSubtitleExt . fileExt) files
  case videos of
    [video] -> do
      let videoExt = fileExt video
          newVideoName = torrent.name <> "." <> videoExt
      renameTorrentFile hash video.name newVideoName
      Log.logInfo_ $ "Rename " <> video.name <> " to " <> newVideoName
      for_ subs $ \sub -> do
        let newSubName = case extractSubLang sub.name of
              Just (lang, subExt) ->
                torrent.name <> "." <> toMediaCode lang <> "." <> subExt
              Nothing ->
                torrent.name <> "." <> fileExt sub
        renameTorrentFile hash sub.name newSubName
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
      transact $ PendingNotificationDB.insertPendingNotification
        PendingNotification {infoHash = hash, title, posterUrl}
    _ ->
      Log.logAttention_ $ "rename: skipping torrent with "
        <> show (length videos) <> " video files: " <> torrent.name

-- | Extract subtitle language and extension from a filename.
--
-- >>> extractSubLang "folder/[Sub] Title - 01.CHS.ass"
-- Just (CHS, "ass")
extractSubLang :: Text -> Maybe (Subtitle, Text)
extractSubLang fileName =
  let baseName = T.takeWhileEnd (/= '/') fileName
      ext = T.toLower $ T.takeWhileEnd (/= '.') baseName
      withoutExt = T.dropEnd (T.length ext + 1) baseName
      langTag = T.takeWhileEnd (/= '.') withoutExt
   in (,ext) <$> fromText langTag
