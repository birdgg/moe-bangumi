-- | DTOs for collection torrent management.
module Moe.Web.API.DTO.Collection
  ( CollectionAddRequest (..),
    CollectionAddResponse (..),
    CollectionFilesResponse (..),
    TorrentFileDTO (..),
    CollectionConfirmRequest (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Moe.Prelude

-- | Request to add a collection torrent (paused).
data CollectionAddRequest = CollectionAddRequest
  { torrentUrl :: Text,
    infoHash :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

-- | Response after adding a collection torrent.
data CollectionAddResponse = CollectionAddResponse
  { hash :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

-- | Response containing the file list for a torrent.
data CollectionFilesResponse = CollectionFilesResponse
  { files :: [TorrentFileDTO]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

-- | A single file within a torrent.
data TorrentFileDTO = TorrentFileDTO
  { index :: Int,
    name :: Text,
    size :: Int64
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

-- | Request to confirm file selection and start download.
data CollectionConfirmRequest = CollectionConfirmRequest
  { hash :: Text,
    unwantedIndices :: [Int]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)
