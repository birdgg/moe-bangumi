module BangumiData.Client
  ( fetchAll,
    BangumiDataResponse (..),
  )
where

import BangumiData.Types (BangumiDataItem)
import Data.Aeson (FromJSON, eitherDecode, parseJSON, withObject, (.:))
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)

newtype BangumiDataResponse = BangumiDataResponse
  { items :: [BangumiDataItem]
  }
  deriving stock (Eq, Show)

instance FromJSON BangumiDataResponse where
  parseJSON = withObject "BangumiDataResponse" $ \v ->
    BangumiDataResponse <$> v .: "items"

bangumiDataUrl :: String
bangumiDataUrl = "https://raw.githubusercontent.com/bangumi-data/bangumi-data/master/dist/data.json"

fetchAll :: IO (Either String [BangumiDataItem])
fetchAll = do
  manager <- newManager tlsManagerSettings
  request <- parseRequest bangumiDataUrl
  response <- httpLbs request manager
  pure $ fmap (.items) $ eitherDecode @BangumiDataResponse (responseBody response)
