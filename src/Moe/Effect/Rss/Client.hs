module Moe.Effect.Rss.Client
  ( fetchRss,
    parseRss,
    parseRssWithSource,
    module Moe.Effect.Rss.Types,
  )
where

import Control.Exception (try)
import Data.ByteString (ByteString, fromStrict, toStrict)
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import Data.Text qualified as T
import Moe.Effect.Rss.Source (SomeRssSource (..), parseItemFor, selectRssSource)
import Moe.Effect.Rss.Types
import Network.HTTP.Client
import Network.HTTP.Types.Status qualified as Status
import Text.XML (def, parseLBS)
import Text.XML.Cursor

fetchRss :: Manager -> Text -> IO (Either RssError [RawItem])
fetchRss manager url = do
  result <- fetchRssXml manager url
  let source = selectRssSource url
  pure $ result >>= parseRssWithSource source

parseRss :: Text -> ByteString -> Either RssError [RawItem]
parseRss url = parseRssWithSource (selectRssSource url)

parseRssWithSource :: SomeRssSource -> ByteString -> Either RssError [RawItem]
parseRssWithSource source xml = parseRssItems source (fromStrict xml)

fetchRssXml :: Manager -> Text -> IO (Either RssError ByteString)
fetchRssXml manager url = do
  result <- try $ do
    request <- parseRequest (T.unpack url)
    let request' =
          request
            { requestHeaders =
                [("Accept", "application/rss+xml, application/xml, text/xml")]
            }
    response <- httpLbs request' manager
    pure (Status.statusCode (responseStatus response), responseBody response)
  case result of
    Left (e :: HttpException) ->
      pure $ Left $ NetworkError $ T.pack (show e)
    Right (status, body)
      | status >= 200 && status < 300 ->
          pure $ Right $ toStrict body
      | otherwise ->
          pure $ Left $ NetworkError $ "HTTP " <> T.pack (show status)

parseRssItems :: SomeRssSource -> LBS.ByteString -> Either RssError [RawItem]
parseRssItems source xml =
  case parseLBS def xml of
    Left err -> Left $ XmlParseError $ T.pack (show err)
    Right doc ->
      let cursor = fromDocument doc
          items = cursor $// element "item"
       in Right $ map (parseItemWith source) items

parseItemWith :: SomeRssSource -> Cursor -> RawItem
parseItemWith (SomeRssSource proxy) = parseItemFor proxy
