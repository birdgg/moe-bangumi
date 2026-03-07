module Moe.Infra.Metadata.BangumiData
  ( fetchBangumiDataMonth,
    matchesKeyword,
    module Moe.Infra.Metadata.BangumiData.Types,
  )
where

import Data.Text qualified as T
import Data.Time.Calendar.Month (Month)
import Data.Time.Calendar.Month qualified as Month
import Moe.Infra.Http.Effect (Http, runServantClient)
import Moe.Infra.Metadata.BangumiData.API (BangumiDataAPI, MonthFile (..))
import Moe.Infra.Metadata.BangumiData.Types
import Moe.Infra.Metadata.Types (MetadataFetchError (..), classifyClientError)
import Moe.Prelude
import Network.HTTP.Types.Status (statusCode)
import Servant.Client (BaseUrl (..), ClientError (..), ClientM, Scheme (..), client)
import Servant.Client.Core (ResponseF (..))

-- | Fetch bangumi-data items for a single month.
fetchBangumiDataMonth :: (Http :> es, IOE :> es) => Month -> Eff es (Either MetadataFetchError [BangumiDataItem])
fetchBangumiDataMonth month = do
  let monthFile = toMonthFile month
  result <- runServantClient bangumiDataBaseUrl (fetchMonthItems monthFile)
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
