module Moe.Effect.BangumiData.API
  ( BangumiDataAPI,
    MonthFile (..),
  )
where

import Data.Aeson (FromJSON, eitherDecode)
import Data.Text qualified as T
import Moe.Effect.BangumiData.Types (BangumiDataItem)
import Network.HTTP.Media ((//))
import Servant.API
import Text.Printf (printf)

data PlainJSON

instance Accept PlainJSON where
  contentType _ = "text" // "plain"

instance (FromJSON a) => MimeUnrender PlainJSON a where
  mimeUnrender _ = eitherDecode

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
    :> Get '[PlainJSON] [BangumiDataItem]
