module Moe.App.Logging
  ( LogDestination (..),
    LogConfig (..),
    defaultLogConfig,
    makeLogger,
    runLog,
    timeAction,
  )
where

import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time.Clock (diffUTCTime, getCurrentTime, nominalDiffTimeToSeconds)
import Effectful
import Effectful.Log (Log, LogLevel (..), Logger)
import Effectful.Log qualified as Log
import Log.Internal.Logger (withLogger)
import Log.Logger (mkLogger)
import System.IO (stdout)

data LogDestination
  = PrettyStdOut
  | JsonFile FilePath
  deriving stock (Eq, Show)

data LogConfig = LogConfig
  { destination :: LogDestination,
    logLevel :: LogLevel
  }
  deriving stock (Eq, Show)

defaultLogConfig :: LogConfig
defaultLogConfig =
  LogConfig
    { destination = PrettyStdOut,
      logLevel = LogInfo
    }

makeLogger :: (IOE :> es) => LogDestination -> (Logger -> Eff es a) -> Eff es a
makeLogger dest action = case dest of
  PrettyStdOut -> withPrettyStdOutBackend action
  JsonFile path -> withJSONFileBackend path action

withPrettyStdOutBackend :: (IOE :> es) => (Logger -> Eff es a) -> Eff es a
withPrettyStdOutBackend action = withRunInIO $ \unlift -> do
  logger <- liftIO $ mkLogger "pretty-stdout" $ \msg ->
    TIO.hPutStrLn stdout (formatLogMessage (Aeson.toJSON msg))
  withLogger logger (unlift . action)

formatLogMessage :: Aeson.Value -> Text
formatLogMessage val = case val of
  Aeson.Object obj ->
    let level = lookupText "level" obj
        component = lookupText "component" obj
        message = lookupText "message" obj
     in "[" <> level <> "] [" <> component <> "] " <> message
  _ -> T.pack (show val)
  where
    lookupText key obj = case KM.lookup key obj of
      Just (Aeson.String t) -> t
      _ -> ""

withJSONFileBackend :: (IOE :> es) => FilePath -> (Logger -> Eff es a) -> Eff es a
withJSONFileBackend path action = withRunInIO $ \unlift -> do
  liftIO $ BS.hPutStrLn stdout $ BS.pack $ "Redirecting logs to " <> path
  logger <- liftIO $ mkLogger "file-json" $ \msg ->
    BS.appendFile path (LBS.toStrict $ Aeson.encode msg <> "\n")
  withLogger logger (unlift . action)

runLog :: (IOE :> es) => Text -> Logger -> LogLevel -> Eff (Log : es) a -> Eff es a
runLog = Log.runLog

timeAction :: (Log :> es, IOE :> es) => Text -> Eff es a -> Eff es a
timeAction label action = do
  start <- liftIO getCurrentTime
  result <- action
  end <- liftIO getCurrentTime
  let elapsed = realToFrac (nominalDiffTimeToSeconds (diffUTCTime end start)) * 1000 :: Double
  Log.logInfo_ $ label <> " took " <> T.pack (show elapsed) <> "ms"
  pure result
