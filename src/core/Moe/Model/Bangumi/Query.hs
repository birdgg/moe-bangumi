module Moe.Model.Bangumi.Query
  ( BangumiQuery (..),
    getBangumi,
    listBangumi,
  )
where

import Effectful
import Effectful.Dispatch.Dynamic
import Moe.Model.Bangumi.Types qualified as Types

data BangumiQuery :: Effect where
  GetBangumi :: Types.BangumiId -> BangumiQuery m (Maybe Types.Bangumi)
  ListBangumi :: BangumiQuery m [Types.Bangumi]

type instance DispatchOf BangumiQuery = Dynamic

getBangumi :: (HasCallStack, BangumiQuery :> es) => Types.BangumiId -> Eff es (Maybe Types.Bangumi)
getBangumi = send . GetBangumi

listBangumi :: (HasCallStack, BangumiQuery :> es) => Eff es [Types.Bangumi]
listBangumi = send ListBangumi
