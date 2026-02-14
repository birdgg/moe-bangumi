-- | Embedded frontend files via Template Haskell.
-- Separate module so TH runs in its own compilation unit.
module Moe.Web.Embedded
  ( embeddedFiles,
    indexHtml,
  )
where

import Data.FileEmbed (embedDir, embedFile)
import Data.Map.Strict qualified as Map
import Moe.Prelude

-- | All files from @web/dist@ embedded at compile time.
embeddedFiles :: Map FilePath ByteString
embeddedFiles = Map.fromList $(embedDir "web/dist")

-- | The SPA entry point, used as fallback for client-side routing.
indexHtml :: ByteString
indexHtml = $(embedFile "web/dist/index.html")
