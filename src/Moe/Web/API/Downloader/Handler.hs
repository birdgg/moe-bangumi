-- | Handler for testing qBittorrent connection.
module Moe.Web.API.Downloader.Handler
  ( handleTestDownloader,
  )
where

import Moe.Infra.Downloader.Effect (testConnection)
import Moe.Prelude
import Moe.Web.API.DTO.Downloader (TestDownloaderRequest (..), TestDownloaderResponse (..))
import Moe.Web.Types (ServerEff)

-- | Test a qBittorrent connection with the given credentials.
handleTestDownloader :: TestDownloaderRequest -> ServerEff TestDownloaderResponse
handleTestDownloader req = do
  result <- testConnection req.url req.username req.password
  pure $ case result of
    Left errMsg ->
      TestDownloaderResponse
        { success = False,
          version = Nothing,
          message = errMsg
        }
    Right ver ->
      TestDownloaderResponse
        { success = True,
          version = Just ver,
          message = "Connected"
        }
