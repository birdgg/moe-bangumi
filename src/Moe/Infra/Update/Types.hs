-- | Domain types for self-update system.
module Moe.Infra.Update.Types
  ( UpdateClientError (..),
    AboutInfo (..),
    Platform (..),
    Arch (..),
    PlatformInfo (..),
    detectPlatform,
    isNewer,
    sameMajorMinor,
  )
where

import Data.Aeson (ToJSON)
import Data.Text qualified as T
import Data.Text.Display (Display (..))
import Data.Time (UTCTime)
import Moe.Prelude
import System.Environment (getExecutablePath)
import System.Info qualified as SysInfo

-- | Structured update client errors.
data UpdateClientError
  = UpdateNetworkError Text
  | UpdateChecksumMismatch Text Text
  | UpdateUnsupportedPlatform Text
  | UpdateExtractionFailed Text
  | UpdateFileError Text
  deriving stock (Show, Eq)

instance Display UpdateClientError where
  displayBuilder = \case
    UpdateNetworkError msg -> "Update network error: " <> displayBuilder msg
    UpdateChecksumMismatch expected actual ->
      "Checksum mismatch: expected " <> displayBuilder expected <> ", got " <> displayBuilder actual
    UpdateUnsupportedPlatform msg -> "Unsupported platform: " <> displayBuilder msg
    UpdateExtractionFailed msg -> "Extraction failed: " <> displayBuilder msg
    UpdateFileError msg -> "File error: " <> displayBuilder msg

-- | Version and update information.
data AboutInfo = AboutInfo
  { currentVersion :: Text,
    latestVersion :: Text,
    needUpdate :: Bool,
    autoUpdate :: Bool,
    downloadUrl :: Text,
    checksumUrl :: Text,
    changelog :: Text,
    publishedAt :: Maybe UTCTime
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

data Platform = Linux | Darwin
  deriving stock (Eq, Show, Enum, Bounded)

instance ToText Platform where
  toText Linux = "linux"
  toText Darwin = "darwin"

data Arch = AMD64 | ARM64
  deriving stock (Eq, Show, Enum, Bounded)

instance ToText Arch where
  toText AMD64 = "amd64"
  toText ARM64 = "arm64"

-- | Current platform information.
data PlatformInfo = PlatformInfo
  { platform :: Platform,
    arch :: Arch,
    executablePath :: FilePath
  }
  deriving stock (Eq, Show)

-- | Detect current OS, architecture, and executable path.
detectPlatform :: IO PlatformInfo
detectPlatform = do
  executablePath <- getExecutablePath
  pure PlatformInfo {platform = detectOS, arch = detectArch, executablePath}
  where
    detectOS :: Platform
    detectOS = case SysInfo.os of
      "darwin" -> Darwin
      _ -> Linux

    detectArch :: Arch
    detectArch = case SysInfo.arch of
      "aarch64" -> ARM64
      "arm64" -> ARM64
      "x86_64" -> AMD64
      _ -> AMD64

-- | Compare two version strings. True if remote is strictly newer.
isNewer :: Text -> Text -> Bool
isNewer remote current =
  case (parseVersion remote, parseVersion current) of
    (Just r, Just c) -> r > c
    _ -> False

-- | Check if two versions share the same major.minor.
sameMajorMinor :: Text -> Text -> Bool
sameMajorMinor a b =
  case (parseVersion a, parseVersion b) of
    (Just (ma : mia : _), Just (mb : mib : _)) -> ma == mb && mia == mib
    _ -> False

-- | Parse "v1.2.3" or "1.2.3-beta" into [1, 2, 3].
-- Strips leading 'v'/'V' prefix and trailing pre-release suffix (e.g. "-beta.1").
parseVersion :: Text -> Maybe [Int]
parseVersion t =
  let stripped = T.dropWhile (\c -> c == 'v' || c == 'V') t
      base = T.takeWhile (/= '-') stripped
      parts = T.splitOn "." base
   in case traverse (readMaybe . toString) parts of
        Just nums@(_ : _) -> Just nums
        _ -> Nothing
