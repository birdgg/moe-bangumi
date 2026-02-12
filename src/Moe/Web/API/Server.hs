module Moe.Web.API.Server where

import Moe.Prelude
import Moe.Web.API.Bangumi.Handler qualified as Bangumi
import Moe.Web.API.Calendar.Handler (handleCalendar)
import Moe.Web.API.Downloader.Handler (handleTestDownloader)
import Moe.Web.API.Log.Handler (handleGetLogs)
import Moe.Web.API.Media.Handler qualified as Media
import Moe.Web.API.Notification.Handler (handleTestNotification)
import Moe.Web.API.Routes qualified as API
import Moe.Web.API.Rss.Handler (handleDownloadTorrent, handleSearchRss)
import Moe.Web.API.Setting.Handler (handleGetSetting, handleUpdateSetting)
import Moe.Web.API.Tracking.Handler qualified as Tracking
import Moe.Web.Types (ServerEff)
import Servant (NoContent (..), ServerT)

apiServer :: ServerT API.Routes ServerEff
apiServer =
  API.Routes'
    { health = pure "ok",
      calendar = handleCalendar,
      getSetting = handleGetSetting,
      updateSetting = handleUpdateSetting,
      listTracking = Tracking.handleListTracking,
      listTrackingWithBangumi = Tracking.handleListTrackingWithBangumi,
      getTracking = Tracking.handleGetTracking,
      createTracking = Tracking.handleCreateTracking,
      updateTracking = Tracking.handleUpdateTracking,
      deleteTracking = Tracking.handleDeleteTracking,
      searchRss = handleSearchRss,
      downloadTorrent = handleDownloadTorrent,
      testDownloader = handleTestDownloader,
      testNotification = handleTestNotification,
      testMedia = Media.handleTestMedia,
      listLibraries = Media.handleListLibraries,
      importLibrary = Media.handleImportLibrary,
      searchTmdb = Bangumi.handleSearchTmdb,
      updateBangumiTmdbId = \bid req -> Bangumi.handleUpdateBangumiTmdbId bid req >> pure NoContent,
      getLogs = handleGetLogs
    }
