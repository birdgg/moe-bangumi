module Moe.Effect.Metadata
  ( Metadata (..),
    searchBgmtv,
    searchTmdb,
    searchBangumiData,
  )
where

import Effectful
import Effectful.TH (makeEffect)
import Moe.Infra.BangumiData.Types (BangumiDataItem)
import Network.Tmdb.Types.Search (MultiSearchResult)
import Web.Bgmtv.Types (Subject)

data Metadata :: Effect where
  SearchBgmtv :: Text -> Maybe Word16 -> Metadata m [Subject]
  SearchTmdb :: Text -> Maybe Word16 -> Metadata m [MultiSearchResult]
  SearchBangumiData :: Text -> Maybe Word16 -> Metadata m [BangumiDataItem]

makeEffect ''Metadata
