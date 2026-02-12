-- | Handler for log query API.
module Moe.Web.API.Log.Handler (handleGetLogs) where

import Data.Aeson qualified as Aeson
import Data.ByteString.Char8 qualified as BS
import Data.Time.Calendar (Day)
import Effectful.Reader.Static (ask)
import Moe.App.Env (MoeConfig (..), MoeEnv (..))
import Moe.App.Logging (logFilePath)
import Moe.Prelude
import Moe.Web.API.DTO.Log (LogEntry, LogsResponse (..))
import Moe.Web.Types (ServerEff)
import System.Directory (doesFileExist)

-- | Query log entries for a given date with pagination.
-- Returns entries in reverse chronological order (newest first).
handleGetLogs :: Day -> Maybe Word32 -> Maybe Word32 -> ServerEff LogsResponse
handleGetLogs date mPage mPageSize = do
  env <- ask @MoeEnv
  let path = logFilePath env.config.dataFolder date
      page = fromMaybe 1 mPage
      pageSize = min 100 (fromMaybe 50 mPageSize)
  liftIO $ readLogFile path page pageSize

readLogFile :: FilePath -> Word32 -> Word32 -> IO LogsResponse
readLogFile path page pageSize = do
  exists <- doesFileExist path
  if not exists
    then pure $ LogsResponse {entries = [], total = 0, page, pageSize}
    else do
      content <- BS.readFile path
      let allLines = filter (not . BS.null) (BS.lines content)
          allEntries = mapMaybe (Aeson.decodeStrict' @LogEntry) allLines
          total = fromIntegral (length allEntries)
          reversed = reverse allEntries
          offset = fromIntegral ((page - 1) * pageSize)
          paginated = take (fromIntegral pageSize) (drop offset reversed)
      pure $ LogsResponse {entries = paginated, total, page, pageSize}
