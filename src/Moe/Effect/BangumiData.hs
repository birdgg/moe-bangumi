module Moe.Effect.BangumiData
  ( BangumiData (..),
    fetchByMonth,
    fetchByMonths,
    runBangumiDataHttp,
    module Moe.Effect.BangumiData.Types,
  )
where

import Control.Concurrent.Async (mapConcurrently)
import Data.Text qualified as T
import Data.Time.Calendar.Month (Month)
import Data.Time.Calendar.Month qualified as Month
import Effectful
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Error.Static (Error)
import Effectful.TH (makeEffect)
import Moe.Effect.BangumiData.API (MonthFile (..))
import Moe.Effect.BangumiData.Client
import Moe.Effect.BangumiData.Types
import Moe.Error (MoeError (..))
import Moe.Prelude (liftEither)
import Network.HTTP.Client (Manager)
import Network.HTTP.Types.Status (statusCode)
import Servant.Client (ClientError (..))
import Servant.Client.Core (ResponseF (..))

data BangumiData :: Effect where
  FetchByMonth :: Month -> BangumiData m [BangumiDataItem]
  FetchByMonths :: [Month] -> BangumiData m [BangumiDataItem]

makeEffect ''BangumiData

runBangumiDataHttp ::
  (IOE :> es, Error MoeError :> es) =>
  Manager ->
  Eff (BangumiData : es) a ->
  Eff es a
runBangumiDataHttp manager = interpret $ \_ -> \case
  FetchByMonth month -> liftEither =<< fetchOne manager month
  FetchByMonths months -> do
    results <- withRunInIO $ \runInIO ->
      mapConcurrently (runInIO . fetchOne manager) months
    liftEither $ concat <$> sequence results

fetchOne :: (IOE :> es) => Manager -> Month -> Eff es (Either MoeError [BangumiDataItem])
fetchOne manager month = handleResult <$> runRequest manager (bangumiDataClient.fetchMonthItems (toMonthFile month))

bangumiDataClient :: BangumiDataClient
bangumiDataClient = mkBangumiDataClient

toMonthFile :: Month -> MonthFile
toMonthFile (Month.YearMonth year month) = MonthFile (fromIntegral year, month)

handleResult :: Either ClientError [BangumiDataItem] -> Either MoeError [BangumiDataItem]
handleResult (Right items) = Right items
handleResult (Left (FailureResponse _ Response{responseStatusCode}))
  | statusCode responseStatusCode == 404 = Right []
handleResult (Left err) = Left $ ExternalApiError $ "BangumiData: " <> T.pack (show err)
