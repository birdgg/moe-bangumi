module Moe.Effect.BangumiData
  ( BangumiData (..),
    fetchBangumiDataByAnimeSeason,
  )
where

import Effectful
import Effectful.Dispatch.Dynamic
import Moe.Adapter.Http.BangumiData.Types (BangumiDataItem)
import Moe.Core.Bangumi.Types (AnimeSeason)

data BangumiData :: Effect where
  FetchBangumiDataByAnimeSeason :: AnimeSeason -> BangumiData m [BangumiDataItem]

type instance DispatchOf BangumiData = Dynamic

fetchBangumiDataByAnimeSeason ::
  (HasCallStack, BangumiData :> es) =>
  AnimeSeason ->
  Eff es [BangumiDataItem]
fetchBangumiDataByAnimeSeason = send . FetchBangumiDataByAnimeSeason
