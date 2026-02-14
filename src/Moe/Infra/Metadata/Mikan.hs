-- | Mikan bangumi search scraper.
module Moe.Infra.Metadata.Mikan
  ( searchMikan,
    parseSearchResults,
    module Moe.Infra.Metadata.Mikan.Types,
  )
where

import Control.Exception (try)
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Moe.Domain.Parser.OriginalTitle (ParsedTitle (..), parseOriginalTitle)
import Moe.Domain.Shared.Metadata (MikanId (..))
import Moe.Infra.Metadata.Mikan.Types
import Moe.Infra.Metadata.Types (Keyword, MetadataFetchError (..))
import Moe.Prelude
import Network.HTTP.Client
import Network.HTTP.Types (urlEncode)
import Network.HTTP.Types.Status qualified as Status
import Text.HTML.DOM qualified as HTML
import Text.XML.Cursor

-- | Search Mikan for bangumi by keyword.
searchMikan :: Manager -> Keyword -> IO (Either MetadataFetchError [MikanSearchResult])
searchMikan manager keyword = do
  result <- fetchSearchPage manager keyword
  pure $ result >>= parseSearchResults

-- | Fetch the Mikan search HTML page.
fetchSearchPage :: Manager -> Keyword -> IO (Either MetadataFetchError LBS.ByteString)
fetchSearchPage manager keyword = do
  let url = "https://mikanani.me/Home/Search?searchstr=" <> encodeUrlParam keyword
  result <- try $ do
    request <- parseRequest (toString url)
    response <- httpLbs request manager
    pure (Status.statusCode (responseStatus response), responseBody response)
  case result of
    Left (e :: HttpException) ->
      pure $ Left $ MetaNetworkError (show e)
    Right (status, body)
      | status >= 200 && status < 300 ->
          pure $ Right body
      | otherwise ->
          pure $ Left $ MetaHttpError status

-- | Parse Mikan search results from HTML.
parseSearchResults :: LBS.ByteString -> Either MetadataFetchError [MikanSearchResult]
parseSearchResults html =
  let doc = HTML.parseLBS html
      cursor = fromDocument doc
      items = orSelf descendant cursor >>= hasClass "an-ul" >>= child >>= element "li"
   in Right $ mapMaybe parseItem items

-- | Parse a single search result item.
parseItem :: Cursor -> Maybe MikanSearchResult
parseItem li = do
  href <- listToMaybe $ li $// element "a" >=> attribute "href"
  mid <- extractMikanId href
  rawTitle <- listToMaybe $ li $// hasClass "an-text" >=> attribute "title"
  guard (not $ T.null rawTitle)
  let parsed = parseOriginalTitle ("", rawTitle)
  pure MikanSearchResult {mikanId = mid, title = parsed.titleChs, season = parsed.season}

-- | Check if an element has a CSS class (handles multi-class attributes).
hasClass :: Text -> Axis
hasClass cls c = [c | any ((cls `elem`) . words) (attribute "class" c)]

-- | Extract MikanId from href like "/Home/Bangumi/3519".
extractMikanId :: Text -> Maybe MikanId
extractMikanId href = do
  suffix <- T.stripPrefix "/Home/Bangumi/" href
  num <- readMaybe (toString suffix)
  pure $ MikanId num

encodeUrlParam :: Text -> Text
encodeUrlParam = decodeUtf8 . urlEncode True . encodeUtf8
