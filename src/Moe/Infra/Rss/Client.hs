module Moe.Infra.Rss.Client
  ( fetchRss,
    parseRss,
    parseRssWithSource,
    module Moe.Infra.Rss.Types,
  )
where

import Control.Exception (try)
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Moe.Prelude
import Moe.Infra.Rss.Source (SomeRssSource (..), parseItemFor, selectRssSource)
import Moe.Infra.Rss.Types
import Network.HTTP.Client
import Network.HTTP.Types.Status qualified as Status
import Text.XML (def, parseLBS)
import Text.XML.Cursor

fetchRss :: Manager -> Text -> IO (Either RssFetchError [RawItem])
fetchRss manager url = do
  result <- fetchRssXml manager url
  let source = selectRssSource url
  pure $ result >>= parseRssWithSource source

parseRss :: Text -> ByteString -> Either RssFetchError [RawItem]
parseRss url = parseRssWithSource (selectRssSource url)

parseRssWithSource :: SomeRssSource -> ByteString -> Either RssFetchError [RawItem]
parseRssWithSource source xml = parseRssItems source (LBS.fromStrict xml)

fetchRssXml :: Manager -> Text -> IO (Either RssFetchError ByteString)
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
      pure $ Left $ RssNetworkError (fmtHttpException e)
    Right (status, body)
      | status >= 200 && status < 300 ->
          pure $ Right $ LBS.toStrict body
      | otherwise ->
          pure $ Left $ RssHttpError status

parseRssItems :: SomeRssSource -> LBS.ByteString -> Either RssFetchError [RawItem]
parseRssItems source xml =
  case parseLBS def xml of
    Left err -> Left $ RssParseError (show err)
    Right doc ->
      let cursor = fromDocument doc
          items = cursor $// element "item"
       in Right $ map (parseItemWith source) items

parseItemWith :: SomeRssSource -> Cursor -> RawItem
parseItemWith (SomeRssSource proxy) = parseItemFor proxy

-- | Format HttpException concisely, omitting verbose Request details.
fmtHttpException :: HttpException -> Text
fmtHttpException = \case
  HttpExceptionRequest req exc ->
    fmtRequestUrl req <> " " <> fmtHttpContent exc
  InvalidUrlException url reason ->
    toText url <> " " <> toText reason

fmtRequestUrl :: Request -> Text
fmtRequestUrl req =
  let scheme = if secure req then "https://" else "http://"
   in scheme <> decodeUtf8 (host req) <> decodeUtf8 (path req) <> decodeUtf8 (queryString req)

fmtHttpContent :: HttpExceptionContent -> Text
fmtHttpContent = \case
  ConnectionTimeout -> "connection timeout"
  ResponseTimeout -> "response timeout"
  ConnectionFailure _ -> "connection failed"
  ConnectionClosed -> "connection closed"
  TooManyRedirects _ -> "too many redirects"
  other -> show other
