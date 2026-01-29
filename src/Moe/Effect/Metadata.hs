module Moe.Effect.Metadata
  ( Metadata (..),
    searchBgmtv,
    searchTmdb,
    searchBangumiData,
  )
where

import Effectful
import Effectful.Dispatch.Dynamic
import Moe.Adapter.Http.BangumiData.Types (BangumiDataItem)
import Network.Tmdb.Types.Search (MultiSearchResult)
import Web.Bgmtv.Types (Subject)

data Metadata :: Effect where
  SearchBgmtv :: Text -> Maybe Word16 -> Metadata m [Subject]
  SearchTmdb :: Text -> Maybe Word16 -> Metadata m [MultiSearchResult]
  SearchBangumiData :: Text -> Maybe Word16 -> Metadata m [BangumiDataItem]

type instance DispatchOf Metadata = Dynamic

searchBgmtv ::
  (HasCallStack, Metadata :> es) =>
  Text ->
  Maybe Word16 ->
  Eff es [Subject]
searchBgmtv keyword year = send (SearchBgmtv keyword year)

searchTmdb ::
  (HasCallStack, Metadata :> es) =>
  Text ->
  Maybe Word16 ->
  Eff es [MultiSearchResult]
searchTmdb keyword year = send (SearchTmdb keyword year)

searchBangumiData ::
  (HasCallStack, Metadata :> es) =>
  Text ->
  Maybe Word16 ->
  Eff es [BangumiDataItem]
searchBangumiData keyword year = send (SearchBangumiData keyword year)
