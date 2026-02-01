module Moe.Effect.BangumiData.API
  ( BangumiDataAPI,
    MonthFile (..),
  )
where

import Data.Text qualified as T
import Moe.Effect.BangumiData.Types (BangumiDataItem)
import Servant.API
import Text.Printf (printf)

newtype MonthFile = MonthFile (Int, Int)

instance ToHttpApiData MonthFile where
  toUrlPiece (MonthFile (year, month)) = T.pack $ printf "%d/%02d.json" year month

type BangumiDataAPI =
  "bangumi-data"
    :> "bangumi-data"
    :> "master"
    :> "data"
    :> "items"
    :> Capture "month_file" MonthFile
    :> Get '[JSON] [BangumiDataItem]
