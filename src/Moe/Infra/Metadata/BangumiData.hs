module Moe.Infra.Metadata.BangumiData
  ( fetchBangumiDataMonth,
    matchesKeyword,
    module Moe.Infra.Metadata.BangumiData.Types,
  )
where

import Data.Text qualified as T
import Data.Time.Calendar.Month (Month)
import Data.Time.Calendar.Month qualified as Month
import Effectful
import Moe.Infra.Metadata.BangumiData.API (BangumiDataAPI, MonthFile (..))
import Moe.Infra.Metadata.BangumiData.Types
import Moe.Infra.Metadata.Types (MetadataFetchError (..), classifyClientError)
import Moe.Prelude
import Network.HTTP.Client (Manager)
import Network.HTTP.Types.Status (statusCode)
import Servant.Client

-- | Fetch bangumi-data items for a single month.
fetchBangumiDataMonth :: (IOE :> es) => Manager -> Month -> Eff es (Either MetadataFetchError [BangumiDataItem])
fetchBangumiDataMonth manager month = do
  let monthFile = toMonthFile month
      env = mkClientEnv manager bangumiDataBaseUrl
  result <- liftIO $ runClientM (fetchMonthItems monthFile) env
  pure $ handleBangumiDataResult result

toMonthFile :: Month -> MonthFile
toMonthFile (Month.YearMonth year month) = MonthFile (fromIntegral year, month)

fetchMonthItems :: MonthFile -> ClientM [BangumiDataItem]
fetchMonthItems = client (Proxy @BangumiDataAPI)

bangumiDataBaseUrl :: BaseUrl
bangumiDataBaseUrl =
  BaseUrl
    { baseUrlScheme = Https,
      baseUrlHost = "raw.githubusercontent.com",
      baseUrlPort = 443,
      baseUrlPath = ""
    }

handleBangumiDataResult :: Either ClientError [BangumiDataItem] -> Either MetadataFetchError [BangumiDataItem]
handleBangumiDataResult (Right items) = Right items
handleBangumiDataResult (Left (FailureResponse _ Response {responseStatusCode}))
  | statusCode responseStatusCode == 404 = Right []
handleBangumiDataResult (Left err) = Left $ classifyClientError err

-- | Check if a BangumiDataItem matches a keyword.
matchesKeyword :: Text -> BangumiDataItem -> Bool
matchesKeyword keyword item =
  let kw = T.toLower keyword
      allTitles = item.title : item.titleTranslate.zhHans <> item.titleTranslate.zhHant
   in any (T.isInfixOf kw . T.toLower) allTitles
