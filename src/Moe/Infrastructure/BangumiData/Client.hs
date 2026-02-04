module Moe.Infrastructure.BangumiData.Client
  ( BangumiDataClient (..),
    mkBangumiDataClient,
    runRequest,
  )
where

import Effectful (Eff, IOE, (:>))
import Moe.Infrastructure.BangumiData.API
import Moe.Infrastructure.BangumiData.Types (BangumiDataItem)
import Moe.Prelude
import Network.HTTP.Client (Manager)
import Servant.Client

newtype BangumiDataClient = BangumiDataClient
  { fetchMonthItems :: MonthFile -> ClientM [BangumiDataItem]
  }

mkBangumiDataClient :: BangumiDataClient
mkBangumiDataClient = BangumiDataClient{fetchMonthItems = client (Proxy @BangumiDataAPI)}

bangumiDataBaseUrl :: BaseUrl
bangumiDataBaseUrl =
  BaseUrl
    { baseUrlScheme = Https,
      baseUrlHost = "raw.githubusercontent.com",
      baseUrlPort = 443,
      baseUrlPath = ""
    }

runRequest :: (IOE :> es) => Manager -> ClientM a -> Eff es (Either ClientError a)
runRequest manager req = liftIO $ runClientM req (mkClientEnv manager bangumiDataBaseUrl)
