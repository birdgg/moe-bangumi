module Moe.Effect.Bangumi
  ( BangumiQuery (..),
    getBangumi,
    listBangumi,
    BangumiUpdate (..),
    createBangumi,
    updateBangumi,
    deleteBangumi,
  )
where

import Effectful
import Effectful.TH (makeEffect)
import Moe.Domain.Bangumi.Types qualified as Types

data BangumiQuery :: Effect where
  GetBangumi :: Types.BangumiId -> BangumiQuery m (Maybe Types.Bangumi)
  ListBangumi :: BangumiQuery m [Types.Bangumi]

makeEffect ''BangumiQuery

data BangumiUpdate :: Effect where
  CreateBangumi :: Types.Bangumi -> BangumiUpdate m Types.BangumiId
  UpdateBangumi :: Types.Bangumi -> BangumiUpdate m ()
  DeleteBangumi :: Types.BangumiId -> BangumiUpdate m ()

makeEffect ''BangumiUpdate
