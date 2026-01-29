module Moe.Effect.BangumiData
  ( BangumiData (..),
    fetchBangumiDataByAnimeSeason,
  )
where

import Effectful
import Effectful.TH (makeEffect)
import Moe.Domain.Bangumi.Types (AnimeSeason)
import Moe.Infra.BangumiData.Types (BangumiDataItem)

data BangumiData :: Effect where
  FetchBangumiDataByAnimeSeason :: AnimeSeason -> BangumiData m [BangumiDataItem]

makeEffect ''BangumiData
