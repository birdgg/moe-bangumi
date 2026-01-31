module Moe.Infra.BangumiData.Client
  ( fetchByMonth,
    fetchByMonths,
  )
where

import Control.Concurrent.Async (mapConcurrently)
import Data.Aeson (eitherDecode)
import Data.Word (Word16)
import Moe.Infra.BangumiData.Types (BangumiDataItem)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)
import Text.Printf (printf)

monthUrl :: Word16 -> Int -> String
monthUrl =
  printf "https://raw.githubusercontent.com/bangumi-data/bangumi-data/master/data/items/%d/%02d.json"

fetchByMonth :: Manager -> Word16 -> Int -> IO (Either String [BangumiDataItem])
fetchByMonth manager year month = do
  request <- parseRequest (monthUrl year month)
  response <- httpLbs request manager
  let status = statusCode $ responseStatus response
  pure $
    if status == 404
      then Right []
      else eitherDecode @[BangumiDataItem] (responseBody response)

fetchByMonths :: Word16 -> [Int] -> IO (Either String [BangumiDataItem])
fetchByMonths year months = do
  manager <- newManager tlsManagerSettings
  results <- mapConcurrently (fetchByMonth manager year) months
  pure $ concat <$> sequence results
