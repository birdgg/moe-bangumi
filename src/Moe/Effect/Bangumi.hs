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
import Effectful.Dispatch.Dynamic
import Moe.Core.Bangumi.Types qualified as Types

data BangumiQuery :: Effect where
  GetBangumi :: Types.BangumiId -> BangumiQuery m (Maybe Types.Bangumi)
  ListBangumi :: BangumiQuery m [Types.Bangumi]

type instance DispatchOf BangumiQuery = Dynamic

getBangumi :: (HasCallStack, BangumiQuery :> es) => Types.BangumiId -> Eff es (Maybe Types.Bangumi)
getBangumi = send . GetBangumi

listBangumi :: (HasCallStack, BangumiQuery :> es) => Eff es [Types.Bangumi]
listBangumi = send ListBangumi

data BangumiUpdate :: Effect where
  CreateBangumi :: Types.Bangumi -> BangumiUpdate m Types.BangumiId
  UpdateBangumi :: Types.Bangumi -> BangumiUpdate m ()
  DeleteBangumi :: Types.BangumiId -> BangumiUpdate m ()

type instance DispatchOf BangumiUpdate = Dynamic

createBangumi :: (HasCallStack, BangumiUpdate :> es) => Types.Bangumi -> Eff es Types.BangumiId
createBangumi = send . CreateBangumi

updateBangumi :: (HasCallStack, BangumiUpdate :> es) => Types.Bangumi -> Eff es ()
updateBangumi = send . UpdateBangumi

deleteBangumi :: (HasCallStack, BangumiUpdate :> es) => Types.BangumiId -> Eff es ()
deleteBangumi = send . DeleteBangumi
