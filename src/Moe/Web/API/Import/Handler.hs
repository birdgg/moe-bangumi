-- | Handler for the import scan API endpoint.
module Moe.Web.API.Import.Handler
  ( handleImportScan,
  )
where

import Moe.Domain.Shared.Entity (Id (..))
import Moe.Job.Import.Scan (ImportResult (..), ImportedEntry (..), SkippedEntry (..), SkipReason (..), importExistingBangumi)
import Moe.Web.API.DTO.Import (ImportScanResponse (..), ImportedBangumiDTO (..), SkippedBangumiDTO (..))
import Moe.Web.Types (ServerEff)
import Moe.Prelude

-- | Handle POST /api/import/scan.
handleImportScan :: ServerEff ImportScanResponse
handleImportScan = do
  result <- importExistingBangumi
  pure $ toResponse result

toResponse :: ImportResult -> ImportScanResponse
toResponse r =
  ImportScanResponse
    { imported = map toImportedDTO r.imported,
      skipped = map toSkippedDTO r.skipped
    }

toImportedDTO :: ImportedEntry -> ImportedBangumiDTO
toImportedDTO e =
  ImportedBangumiDTO
    { bangumiId = coerce e.bangumiId,
      title = e.title,
      posterUrl = e.posterUrl
    }

toSkippedDTO :: SkippedEntry -> SkippedBangumiDTO
toSkippedDTO e =
  SkippedBangumiDTO
    { folderName = e.folderName,
      reason = skipReasonText e.reason
    }

skipReasonText :: SkipReason -> Text
skipReasonText AlreadyTracked = "already tracked"
skipReasonText NoVideoFiles = "no video files found"
skipReasonText TmdbSearchFailed = "TMDB search failed"
