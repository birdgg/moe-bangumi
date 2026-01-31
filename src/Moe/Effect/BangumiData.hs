module Moe.Effect.BangumiData
  ( BangumiData (..),
    fetchByMonth,
    fetchByMonths,
    runBangumiDataHttp,
  )
where

import Control.Concurrent.Async (mapConcurrently)
import Data.Word (Word16)
import Effectful
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.TH (makeEffect)
import Moe.Infra.BangumiData.Client qualified as Client
import Moe.Infra.BangumiData.Types (BangumiDataItem)
import Network.HTTP.Client (Manager)

data BangumiData :: Effect where
  FetchByMonth :: Word16 -> Int -> BangumiData m (Either String [BangumiDataItem])
  FetchByMonths :: Word16 -> [Int] -> BangumiData m (Either String [BangumiDataItem])

makeEffect ''BangumiData

runBangumiDataHttp ::
  (IOE :> es) =>
  Manager ->
  Eff (BangumiData : es) a ->
  Eff es a
runBangumiDataHttp manager = interpret $ \_ -> \case
  FetchByMonth year month ->
    liftIO $ Client.fetchByMonth manager year month
  FetchByMonths year months -> liftIO $ do
    results <- mapConcurrently (Client.fetchByMonth manager year) months
    pure $ concat <$> sequence results
