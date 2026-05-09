module Moe.Libs.QBittorrent.Types
  ( QBConfig (..),
    QBClient (..),
    Credential (..),
    QBClientError (..),
    QBResponseError (..),
    clientErrorToQBClientError,
    InfoHash (..),
    hashesToText,
    Tag (..),
    tagsToText,
    textToTags,
    TorrentFilter (..),
    torrentFilterToText,
    TorrentInfoRequest (..),
    TorrentInfo (..),
    TorrentContent (..),
    TorrentState (..),
    AddTorrentRequest (..),
    LoginForm (..),
    HashesForm (..),
    DeleteTorrentsForm (..),
    TagsForm (..),
    RenameFileForm (..),
    RenameFolderForm (..),
    SetLocationForm (..),
    FilePrioForm (..),
  )
where

import Data.Aeson (FromJSON (..), FromJSONKey, ToJSON (..), ToJSONKey, withObject, withText, (.:), (.:?), (.!=))
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TLE
import Moe.Prelude
import Network.HTTP.Client (CookieJar)
import Servant.Client (ClientEnv, ClientError (..))
import Servant.Client.Core (ResponseF (..))
import Web.FormUrlEncoded (Form (..), ToForm (..))
import Web.HttpApiData (FromHttpApiData, ToHttpApiData)

data QBConfig = QBConfig
  { host :: Text,
    port :: Int,
    credential :: Credential,
    useTLS :: Bool
  }
  deriving stock (Show, Eq, Generic)

data QBClient = QBClient
  { clientEnv :: ClientEnv,
    cookieJar :: TVar CookieJar,
    config :: QBConfig
  }

data Credential = Credential
  { username :: Text,
    password :: Text
  }
  deriving stock (Show, Eq, Generic)

data QBResponseError = QBResponseError
  { statusCode :: Int,
    responseBody :: Text
  }
  deriving stock (Show, Eq, Generic)

data QBClientError
  = ServantError ClientError
  | QBApiError QBResponseError
  deriving stock (Show, Eq)

clientErrorToQBClientError :: ClientError -> QBClientError
clientErrorToQBClientError = \case
  FailureResponse _ Response {responseStatusCode, responseBody} ->
    QBApiError
      QBResponseError
        { statusCode = fromEnum responseStatusCode,
          responseBody = TL.toStrict $ TLE.decodeUtf8With lenientDecode responseBody
        }
  err -> ServantError err

newtype InfoHash = InfoHash {unInfoHash :: Text}
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (IsString, FromJSON, ToJSON, FromJSONKey, ToJSONKey, ToHttpApiData, FromHttpApiData)

hashesToText :: [InfoHash] -> Text
hashesToText = T.intercalate "|" . map (.unInfoHash)

newtype Tag = Tag {unTag :: Text}
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (IsString, FromJSON, ToJSON)

tagsToText :: [Tag] -> Text
tagsToText = T.intercalate "," . map (.unTag)

textToTags :: Text -> [Tag]
textToTags t
  | T.null (T.strip t) = []
  | otherwise = map (Tag . T.strip) $ T.splitOn "," t

data TorrentFilter
  = FilterAll
  | FilterDownloading
  | FilterSeeding
  | FilterCompleted
  | FilterStopped
  | FilterActive
  | FilterInactive
  | FilterRunning
  | FilterStalled
  | FilterStalledUploading
  | FilterStalledDownloading
  | FilterErrored
  deriving stock (Show, Eq, Generic)

torrentFilterToText :: TorrentFilter -> Text
torrentFilterToText = \case
  FilterAll -> "all"
  FilterDownloading -> "downloading"
  FilterSeeding -> "seeding"
  FilterCompleted -> "completed"
  FilterStopped -> "stopped"
  FilterActive -> "active"
  FilterInactive -> "inactive"
  FilterRunning -> "running"
  FilterStalled -> "stalled"
  FilterStalledUploading -> "stalled_uploading"
  FilterStalledDownloading -> "stalled_downloading"
  FilterErrored -> "errored"

data TorrentInfoRequest = TorrentInfoRequest
  { filter_ :: Maybe TorrentFilter,
    category :: Maybe Text,
    tag :: Maybe Tag,
    hashes :: Maybe [InfoHash]
  }
  deriving stock (Show, Eq, Generic)

data TorrentInfo = TorrentInfo
  { hash :: InfoHash,
    name :: Text,
    magnetUri :: Text,
    state :: TorrentState,
    progress :: Double,
    eta :: Int64,
    size :: Int64,
    totalSize :: Int64,
    completed :: Int64,
    amountLeft :: Int64,
    availability :: Double,
    savePath :: Text,
    contentPath :: Text,
    addedOn :: Int64,
    completionOn :: Int64,
    lastActivity :: Int64,
    seenComplete :: Int64,
    timeActive :: Int64,
    downloaded :: Int64,
    downloadedSession :: Int64,
    dlspeed :: Int64,
    uploaded :: Int64,
    uploadedSession :: Int64,
    upspeed :: Int64,
    ratio :: Double,
    ratioLimit :: Double,
    maxRatio :: Double,
    seedingTime :: Int64,
    seedingTimeLimit :: Int64,
    maxSeedingTime :: Int64,
    dlLimit :: Int64,
    upLimit :: Int64,
    numSeeds :: Int64,
    numComplete :: Int64,
    numLeechs :: Int64,
    numIncomplete :: Int64,
    priority :: Int64,
    autoTmm :: Bool,
    flPiecePrio :: Bool,
    forceStart :: Bool,
    seqDl :: Bool,
    superSeeding :: Bool,
    isPrivate :: Bool,
    tracker :: Text,
    reannounce :: Int64,
    category :: Text,
    tags :: [Tag]
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON TorrentInfo where
  parseJSON = withObject "TorrentInfo" $ \o ->
    TorrentInfo
      <$> o .: "hash"
      <*> o .: "name"
      <*> o .:? "magnet_uri" .!= ""
      <*> o .: "state"
      <*> o .: "progress"
      <*> o .: "eta"
      <*> o .: "size"
      <*> o .: "total_size"
      <*> o .: "completed"
      <*> o .: "amount_left"
      <*> o .: "availability"
      <*> o .: "save_path"
      <*> o .: "content_path"
      <*> o .: "added_on"
      <*> o .: "completion_on"
      <*> o .: "last_activity"
      <*> o .: "seen_complete"
      <*> o .: "time_active"
      <*> o .: "downloaded"
      <*> o .: "downloaded_session"
      <*> o .: "dlspeed"
      <*> o .: "uploaded"
      <*> o .: "uploaded_session"
      <*> o .: "upspeed"
      <*> o .: "ratio"
      <*> o .: "ratio_limit"
      <*> o .: "max_ratio"
      <*> o .: "seeding_time"
      <*> o .: "seeding_time_limit"
      <*> o .: "max_seeding_time"
      <*> o .: "dl_limit"
      <*> o .: "up_limit"
      <*> o .: "num_seeds"
      <*> o .: "num_complete"
      <*> o .: "num_leechs"
      <*> o .: "num_incomplete"
      <*> o .: "priority"
      <*> o .: "auto_tmm"
      <*> o .: "f_l_piece_prio"
      <*> o .: "force_start"
      <*> o .: "seq_dl"
      <*> o .: "super_seeding"
      <*> o .:? "isPrivate" .!= False
      <*> o .: "tracker"
      <*> o .: "reannounce"
      <*> o .:? "category" .!= ""
      <*> (textToTags <$> o .:? "tags" .!= "")

data TorrentContent = TorrentContent
  { index :: Int,
    name :: Text,
    size :: Int64,
    progress :: Double,
    priority :: Int,
    isSeed :: Bool
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON TorrentContent where
  parseJSON = withObject "TorrentContent" $ \o ->
    TorrentContent
      <$> o .: "index"
      <*> o .: "name"
      <*> o .: "size"
      <*> o .: "progress"
      <*> o .:? "priority" .!= 1
      <*> o .:? "is_seed" .!= False

data TorrentState
  = StateError
  | MissingFiles
  | Uploading
  | PausedUP
  | StoppedUP
  | QueuedUP
  | StalledUP
  | CheckingUP
  | ForcedUP
  | Allocating
  | Downloading
  | MetaDL
  | PausedDL
  | StoppedDL
  | QueuedDL
  | StalledDL
  | CheckingDL
  | ForcedDL
  | CheckingResumeData
  | Moving
  | StateUnknown
  deriving stock (Show, Eq, Generic)

instance FromJSON TorrentState where
  parseJSON = withText "TorrentState" $ \case
    "error" -> pure StateError
    "missingFiles" -> pure MissingFiles
    "uploading" -> pure Uploading
    "pausedUP" -> pure PausedUP
    "stoppedUP" -> pure StoppedUP
    "queuedUP" -> pure QueuedUP
    "stalledUP" -> pure StalledUP
    "checkingUP" -> pure CheckingUP
    "forcedUP" -> pure ForcedUP
    "allocating" -> pure Allocating
    "downloading" -> pure Downloading
    "metaDL" -> pure MetaDL
    "pausedDL" -> pure PausedDL
    "stoppedDL" -> pure StoppedDL
    "queuedDL" -> pure QueuedDL
    "stalledDL" -> pure StalledDL
    "checkingDL" -> pure CheckingDL
    "forcedDL" -> pure ForcedDL
    "checkingResumeData" -> pure CheckingResumeData
    "moving" -> pure Moving
    _ -> pure StateUnknown

data AddTorrentRequest = AddTorrentRequest
  { urls :: Maybe Text,
    savepath :: Maybe Text,
    category :: Maybe Text,
    tags :: Maybe [Tag],
    rename :: Maybe Text,
    stopped :: Maybe Bool
  }
  deriving stock (Show, Eq, Generic)

instance ToForm AddTorrentRequest where
  toForm req =
    Form $
      Map.fromList $
        catMaybes
          [ fmap (\v -> ("urls", [v])) req.urls,
            fmap (\v -> ("savepath", [v])) req.savepath,
            fmap (\v -> ("category", [v])) req.category,
            fmap (\ts -> ("tags", [tagsToText ts])) req.tags,
            fmap (\v -> ("rename", [v])) req.rename,
            fmap (\v -> ("stopped", [if v then "true" else "false"])) req.stopped
          ]

data LoginForm = LoginForm
  { username :: Text,
    password :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToForm)

newtype HashesForm = HashesForm {hashes :: [InfoHash]}
  deriving stock (Show, Eq, Generic)

instance ToForm HashesForm where
  toForm form =
    Form $
      Map.fromList
        [ ("hashes", [hashesToText form.hashes])
        ]

data DeleteTorrentsForm = DeleteTorrentsForm
  { hashes :: [InfoHash],
    deleteFiles :: Bool
  }
  deriving stock (Show, Eq, Generic)

instance ToForm DeleteTorrentsForm where
  toForm form =
    Form $
      Map.fromList
        [ ("hashes", [hashesToText form.hashes]),
          ("deleteFiles", [if form.deleteFiles then "true" else "false"])
        ]

data TagsForm = TagsForm
  { hashes :: [InfoHash],
    tags :: [Tag]
  }
  deriving stock (Show, Eq, Generic)

instance ToForm TagsForm where
  toForm form =
    Form $
      Map.fromList
        [ ("hashes", [hashesToText form.hashes]),
          ("tags", [tagsToText form.tags])
        ]

data RenameFileForm = RenameFileForm
  { hash :: InfoHash,
    oldPath :: Text,
    newPath :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToForm)

data RenameFolderForm = RenameFolderForm
  { hash :: InfoHash,
    oldPath :: Text,
    newPath :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToForm)

data SetLocationForm = SetLocationForm
  { hashes :: [InfoHash],
    location :: Text
  }
  deriving stock (Show, Eq, Generic)

instance ToForm SetLocationForm where
  toForm form =
    Form $
      Map.fromList
        [ ("hashes", [hashesToText form.hashes]),
          ("location", [form.location])
        ]

data FilePrioForm = FilePrioForm
  { hash :: InfoHash,
    id :: Text,
    priority :: Int
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToForm)
