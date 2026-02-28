-- | DTOs for download manager endpoints.
module Moe.Web.API.DTO.Download
  ( DownloadItem (..),
    TorrentActionRequest (..),
    toDownloadItem,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Moe.Infra.Downloader.Types (InfoHash (..), Tag (..), TorrentInfo (..), TorrentState (..))
import Moe.Prelude

-- | A download item returned to the frontend.
data DownloadItem = DownloadItem
  { hash :: Text,
    name :: Text,
    size :: Int64,
    progress :: Double,
    state :: Text,
    ratio :: Double,
    dlspeed :: Int64,
    upspeed :: Int64,
    eta :: Int64,
    addedOn :: Int64,
    tags :: [Text]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

-- | Request body for pause/resume/delete actions.
data TorrentActionRequest = TorrentActionRequest
  { hashes :: [Text],
    deleteFiles :: Maybe Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

-- | Serialize TorrentState as the qBittorrent API string.
stateToText :: TorrentState -> Text
stateToText = \case
  StateError -> "error"
  MissingFiles -> "missingFiles"
  Uploading -> "uploading"
  PausedUP -> "pausedUP"
  QueuedUP -> "queuedUP"
  StalledUP -> "stalledUP"
  CheckingUP -> "checkingUP"
  ForcedUP -> "forcedUP"
  Allocating -> "allocating"
  Downloading -> "downloading"
  MetaDL -> "metaDL"
  PausedDL -> "pausedDL"
  QueuedDL -> "queuedDL"
  StalledDL -> "stalledDL"
  CheckingDL -> "checkingDL"
  ForcedDL -> "forcedDL"
  CheckingResumeData -> "checkingResumeData"
  Moving -> "moving"
  StoppedUP -> "stoppedUP"
  StoppedDL -> "stoppedDL"
  StateUnknown -> "unknown"

-- | Convert a 'TorrentInfo' into a 'DownloadItem'.
toDownloadItem :: TorrentInfo -> DownloadItem
toDownloadItem t =
  let (InfoHash h) = t.hash
   in DownloadItem
        { hash = h,
          name = t.name,
          size = t.size,
          progress = t.progress,
          state = stateToText t.state,
          ratio = t.ratio,
          dlspeed = t.dlspeed,
          upspeed = t.upspeed,
          eta = t.eta,
          addedOn = t.addedOn,
          tags = map (\(Tag tag) -> tag) t.tags
        }
