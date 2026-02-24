-- | Types for Mikan search results.
module Moe.Infra.Metadata.Mikan.Types
  ( MikanSearchResult (..),
  )
where

import Moe.Domain.Bangumi (SeasonIndex)
import Moe.Domain.Shared.Metadata (MikanId)
import Moe.Prelude

-- | A search result from Mikan.
data MikanSearchResult = MikanSearchResult
  { mikanId :: MikanId,
    title :: Text,
    season :: Maybe SeasonIndex
  }
  deriving stock (Eq, Show)
