{-# LANGUAGE AllowAmbiguousTypes #-}

module Moe.Infrastructure.Rss.Source
  ( RssSource (..),
    SomeRssSource (..),
    Nyaa,
    AcgRip,
    StandardRss,
    selectRssSource,
    generateSearchUrl,
    getChildText,
    extractInfoHashFromUrl,
    extractInfoHashFromMagnet,
    extractInfoHash,
  )
where

import Data.Char (isHexDigit)
import Data.Text qualified as T
import Moe.Prelude
import Moe.Infrastructure.Rss.Types (RawItem (..))
import Network.HTTP.Types (urlEncode)
import Text.XML (Name (..))
import Text.XML.Cursor

class RssSource s where
  parseItemFor :: proxy s -> Cursor -> RawItem
  searchUrlFor :: proxy s -> Text -> Maybe Text
  searchUrlFor _ _ = Nothing

data SomeRssSource = forall s. (RssSource s) => SomeRssSource (Proxy s)

data Nyaa

data AcgRip

data StandardRss

selectRssSource :: Text -> SomeRssSource
selectRssSource url
  | "nyaa.si" `T.isInfixOf` url = SomeRssSource (Proxy @Nyaa)
  | "acg.rip" `T.isInfixOf` url = SomeRssSource (Proxy @AcgRip)
  | otherwise = SomeRssSource (Proxy @StandardRss)

generateSearchUrl :: SomeRssSource -> Text -> Maybe Text
generateSearchUrl (SomeRssSource proxy) = searchUrlFor proxy

instance RssSource StandardRss where
  parseItemFor _ = parseStandardRssItem

instance RssSource AcgRip where
  parseItemFor _ = parseStandardRssItem
  searchUrlFor _ keyword = Just $ "https://acg.rip/.xml?term=" <> encodeUrlParam keyword

parseStandardRssItem :: Cursor -> RawItem
parseStandardRssItem cursor =
  let url = getEnclosureUrl cursor <|> getChildText "link" cursor
   in RawItem
        { title = getChildText "title" cursor,
          pubDate = getChildText "pubDate" cursor,
          torrentUrl = url,
          infoHash = url >>= extractInfoHash
        }

instance RssSource Nyaa where
  parseItemFor _ cursor =
    let url = getEnclosureUrl cursor <|> getChildText "link" cursor
     in RawItem
          { title = getChildText "title" cursor,
            pubDate = getChildText "pubDate" cursor,
            torrentUrl = url,
            infoHash = getNyaaText "infoHash" cursor <|> (url >>= extractInfoHash)
          }
   where
    getNyaaText :: Text -> Cursor -> Maybe Text
    getNyaaText localName c =
      case c $/ element nyaaName &/ content of
        [] -> Nothing
        (t : _) -> Just t
     where
      nyaaNs :: Maybe Text
      nyaaNs = Just "https://nyaa.si/xmlns/nyaa"

      nyaaName :: Name
      nyaaName = Name localName nyaaNs Nothing

  searchUrlFor _ keyword = Just $ "https://nyaa.si/?page=rss&q=" <> encodeUrlParam keyword

getChildText :: Text -> Cursor -> Maybe Text
getChildText name cursor =
  case cursor $/ element (fromText name) &/ content of
    [] -> Nothing
    (t : _) -> Just t
 where
  fromText :: Text -> Name
  fromText t = Name t Nothing Nothing

getEnclosureUrl :: Cursor -> Maybe Text
getEnclosureUrl cursor =
  case cursor $/ element "enclosure" of
    [] -> Nothing
    (enc : _) -> listToMaybe $ attribute "url" enc

extractInfoHashFromUrl :: Text -> Maybe Text
extractInfoHashFromUrl url =
  let filename = T.takeWhileEnd (/= '/') url
      base = fromMaybe filename $ T.stripSuffix ".torrent" filename
   in if T.length base == 40 && T.all isHexDigit base
        then Just (T.toLower base)
        else Nothing

extractInfoHashFromMagnet :: Text -> Maybe Text
extractInfoHashFromMagnet url
  | not ("magnet:?" `T.isPrefixOf` url) = Nothing
  | otherwise =
      let params = T.split (== '&') (T.drop 8 url)
          btihParam = find ("xt=urn:btih:" `T.isPrefixOf`) params
       in case btihParam of
            Nothing -> Nothing
            Just param ->
              let hash = T.drop 12 param
                  cleanHash = T.takeWhile (/= '&') hash
               in if T.length cleanHash == 40 && T.all isHexDigit cleanHash
                    then Just (T.toLower cleanHash)
                    else Nothing

extractInfoHash :: Text -> Maybe Text
extractInfoHash url =
  extractInfoHashFromMagnet url <|> extractInfoHashFromUrl url

encodeUrlParam :: Text -> Text
encodeUrlParam = decodeUtf8 . urlEncode True . encodeUtf8
