module Moe.Model.Bangumi.Update
  ( BangumiUpdate (..),
    createBangumi,
    updateBangumi,
    deleteBangumi,
  )
where

import Effectful
import Effectful.Dispatch.Dynamic
import Moe.Model.Bangumi.Types qualified as Types

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
