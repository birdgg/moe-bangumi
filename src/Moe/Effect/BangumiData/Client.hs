module Moe.Effect.BangumiData.Client
  ( BangumiDataClient (..),
    mkBangumiDataClient,
    runRequest,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.Proxy (Proxy (..))
import Effectful (Eff, IOE, (:>))
import Moe.Effect.BangumiData.API
import Moe.Effect.BangumiData.Types (BangumiDataItem)
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
