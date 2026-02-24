-- | DTOs for the import scan API endpoint.
module Moe.Web.API.DTO.Import
  ( ImportScanResponse (..),
    ImportedBangumiDTO (..),
    SkippedBangumiDTO (..),
  )
where

import Data.Aeson (ToJSON)
import Data.OpenApi (ToSchema)
import Moe.Prelude

-- | Response for the import scan endpoint.
data ImportScanResponse = ImportScanResponse
  { imported :: [ImportedBangumiDTO],
    skipped :: [SkippedBangumiDTO]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, ToSchema)

-- | A successfully imported bangumi.
data ImportedBangumiDTO = ImportedBangumiDTO
  { bangumiId :: Int64,
    title :: Text,
    posterUrl :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, ToSchema)

-- | A bangumi folder that was skipped during import.
data SkippedBangumiDTO = SkippedBangumiDTO
  { folderName :: Text,
    reason :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, ToSchema)
