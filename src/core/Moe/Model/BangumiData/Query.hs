module Moe.Model.BangumiData.Query
  ( BangumiData (..),
    fetchBangumiDataByAnimeSeason,
  )
where

import BangumiData.Types (BangumiDataItem)
import Effectful
import Effectful.Dispatch.Dynamic
import Moe.Model.Bangumi.Types (AnimeSeason)

data BangumiData :: Effect where
  FetchBangumiDataByAnimeSeason :: AnimeSeason -> BangumiData m [BangumiDataItem]

type instance DispatchOf BangumiData = Dynamic

fetchBangumiDataByAnimeSeason ::
  (HasCallStack, BangumiData :> es) =>
  AnimeSeason ->
  Eff es [BangumiDataItem]
fetchBangumiDataByAnimeSeason = send . FetchBangumiDataByAnimeSeason
