-- | Rename job logic for moving and renaming completed torrents.
module Moe.App.Rename.Run
  ( runRename,
  )
where

import Data.Text qualified as T
import Effectful
import Effectful.Exception (try)
import Effectful.Log (Log)
import Effectful.Log qualified as Log
import Moe.Domain.Setting.Types (DownloaderConfig (..), UserPreference (downloader))
import Moe.Infrastructure.Download.Effect
import Moe.Infrastructure.Notification.Effect (Notification, sendNotification)
import Moe.Infrastructure.Setting.Effect (Setting, getSetting)
import Moe.Prelude
import System.FilePath (takeDirectory, takeExtension, (</>))

-- | Run the rename process for all completed torrents with rename tag.
runRename ::
  (Download :> es, Notification :> es, Setting :> es, Log :> es, IOE :> es) =>
  Eff es ()
runRename = do
  torrents <- getRenameTorrents
  let completed = filter isCompleted torrents
  pref <- getSetting
  case pref.downloader of
    Nothing -> Log.logAttention_ "rename: downloader config missing"
    Just dlCfg ->
      forM_ completed $ \torrent -> do
        let hash = infoHashToText torrent.hash
            tags = torrent.tags
        when (subscriptionTag `elem` tags) $ do
          result <- try @SomeException $ renameSubscription dlCfg torrent hash
          case result of
            Left ex ->
              Log.logAttention_ $
                "rename: failed for " <> torrent.name <> " - " <> toText (displayException ex)
            Right () -> pass

-- | Rename and move a subscription torrent from tmp to save path.
renameSubscription ::
  (Download :> es, Notification :> es, Log :> es, IOE :> es) =>
  DownloaderConfig ->
  TorrentInfo ->
  Text ->
  Eff es ()
renameSubscription dlCfg torrent hash = do
  files <- getTorrentFiles hash
  case files of
    [file] -> do
      let ext = toText $ takeExtension (toString file.name)
          newFileName = torrent.name <> ext
          targetLocation = computeTargetLocation dlCfg torrent.savePath
      -- Rename file within torrent
      renameTorrentFile hash file.name newFileName
      -- Move to final location
      setTorrentLocation [hash] targetLocation
      -- Remove rename tag
      removeTagsFromTorrents [hash] [renameTag]
      -- Notify (non-critical)
      notifySafe torrent.name
      Log.logInfo_ $ "Renamed: " <> torrent.name
    _ ->
      Log.logAttention_ $ "rename: skipping multi-file torrent " <> torrent.name

-- | Send notification, logging on failure without propagating the error.
notifySafe ::
  (Notification :> es, Log :> es, IOE :> es) =>
  Text ->
  Eff es ()
notifySafe name = do
  result <- try @SomeException $ sendNotification ("Rename: " <> name) Nothing
  case result of
    Left ex ->
      Log.logAttention_ $ "rename: notification failed - " <> toText (displayException ex)
    Right () -> pass

-- | Compute target location by replacing tmp prefix with save path.
--
-- @
-- Tmp path:    /media/tmp/Bangumi (2025)/Season 01/
-- Save path:   /media/bangumi
-- Result:      /media/bangumi/Bangumi (2025)/Season 01/
-- @
computeTargetLocation :: DownloaderConfig -> Text -> Text
computeTargetLocation dlCfg currentPath =
  let tmpBase = toText $ takeDirectory (toString dlCfg.savePath) </> "tmp"
   in case T.stripPrefix tmpBase currentPath of
        Just relative -> dlCfg.savePath <> relative
        Nothing -> dlCfg.savePath
