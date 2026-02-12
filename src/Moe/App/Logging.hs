-- | Logging configuration and dual-backend logger (stdout + daily JSON files).
module Moe.App.Logging
  ( LogConfig (..),
    defaultLogConfig,
    makeLogger,
    runLog,
    timeAction,
    logFilePath,
  )
where

import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Char8 qualified as BS
import Data.Text.IO qualified as TIO
import Data.Time.Calendar (Day, showGregorian)
import Data.Time.Clock (diffUTCTime, getCurrentTime, nominalDiffTimeToSeconds, utctDay)
import Effectful
import Effectful.Log (LogLevel (..), Logger)
import Effectful.Log qualified as Log
import Log.Internal.Logger (withLogger)
import Log.Logger (mkLogger)
import Moe.Prelude
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

data LogConfig = LogConfig
  { logLevel :: LogLevel
  }
  deriving stock (Eq, Show)

defaultLogConfig :: LogConfig
defaultLogConfig =
  LogConfig
    { logLevel = LogInfo
    }

-- | Create a dual-backend logger that writes to both stdout (pretty) and
-- daily JSON files under @dataFolder/logs/YYYY-MM-DD.log@.
makeLogger :: (IOE :> es) => FilePath -> (Logger -> Eff es a) -> Eff es a
makeLogger dataFolder action = withRunInIO $ \unlift -> do
  let logsDir = dataFolder </> "logs"
  createDirectoryIfMissing True logsDir
  logger <- liftIO $ mkLogger "moe-bangumi" $ \msg -> do
    -- Pretty output to stdout
    TIO.hPutStrLn stdout (formatLogMessage (Aeson.toJSON msg))
    -- JSON line to daily file
    today <- utctDay <$> getCurrentTime
    let path = logsDir </> showGregorian today <> ".log"
    BS.appendFile path (toStrict $ Aeson.encode msg <> "\n")
  withLogger logger (unlift . action)

-- | Compute the log file path for a given date.
logFilePath :: FilePath -> Day -> FilePath
logFilePath dataFolder day =
  dataFolder </> "logs" </> showGregorian day <> ".log"

formatLogMessage :: Aeson.Value -> Text
formatLogMessage val = case val of
  Aeson.Object obj ->
    let level = lookupText "level" obj
        component = lookupText "component" obj
        message = lookupText "message" obj
        dataStr = case KM.lookup "data" obj of
          Just (Aeson.Object d) | not (KM.null d) -> " " <> decodeUtf8 (toStrict $ Aeson.encode d)
          _ -> ""
     in "[" <> level <> "] [" <> component <> "] " <> message <> dataStr
  _ -> show val
  where
    lookupText key obj = case KM.lookup key obj of
      Just (Aeson.String t) -> t
      _ -> ""

runLog :: (IOE :> es) => Text -> Logger -> LogLevel -> Eff (Log : es) a -> Eff es a
runLog = Log.runLog

timeAction :: (Log :> es, IOE :> es) => Text -> Eff es a -> Eff es a
timeAction label action = do
  start <- liftIO getCurrentTime
  result <- action
  end <- liftIO getCurrentTime
  let elapsed = realToFrac (nominalDiffTimeToSeconds (diffUTCTime end start)) * 1000 :: Double
  Log.logInfo_ $ label <> " took " <> show elapsed <> "ms"
  pure result
