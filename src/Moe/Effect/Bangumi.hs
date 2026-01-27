module Moe.Effect.Bangumi
  ( Bangumi (..),
    getBangumi,
    listBangumi,
    createBangumi,
    updateBangumi,
    deleteBangumi,
  )
where

import Effectful
import Effectful.Dispatch.Dynamic
import Moe.Core.Model.Bangumi qualified as Model

data Bangumi :: Effect where
  GetBangumi :: Model.BangumiId -> Bangumi m (Maybe Model.Bangumi)
  ListBangumi :: Bangumi m [Model.Bangumi]
  CreateBangumi :: Model.Bangumi -> Bangumi m Model.BangumiId
  UpdateBangumi :: Model.Bangumi -> Bangumi m ()
  DeleteBangumi :: Model.BangumiId -> Bangumi m ()

type instance DispatchOf Bangumi = Dynamic

getBangumi :: (HasCallStack, Bangumi :> es) => Model.BangumiId -> Eff es (Maybe Model.Bangumi)
getBangumi = send . GetBangumi

listBangumi :: (HasCallStack, Bangumi :> es) => Eff es [Model.Bangumi]
listBangumi = send ListBangumi

createBangumi :: (HasCallStack, Bangumi :> es) => Model.Bangumi -> Eff es Model.BangumiId
createBangumi = send . CreateBangumi

updateBangumi :: (HasCallStack, Bangumi :> es) => Model.Bangumi -> Eff es ()
updateBangumi = send . UpdateBangumi

deleteBangumi :: (HasCallStack, Bangumi :> es) => Model.BangumiId -> Eff es ()
deleteBangumi = send . DeleteBangumi
