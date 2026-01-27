module Moe.Bangumi.Internal.BangumiFile
  ( BangumiMeta (..),
    BangumiFile (..),
  )
where

import Moe.Bangumi.Internal.Content (BangumiContent, TmdbId, Year)
import Moe.Bangumi.Internal.Extension (FileType)

data BangumiMeta = BangumiMeta
  { name :: Text,
    year :: Maybe Year,
    tmdbId :: Maybe TmdbId
  }
  deriving stock (Eq, Show)

data BangumiFile = BangumiFile
  { meta :: BangumiMeta,
    content :: BangumiContent,
    fileType :: FileType
  }
  deriving stock (Eq, Show)
