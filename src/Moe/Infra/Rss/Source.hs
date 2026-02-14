{-# LANGUAGE AllowAmbiguousTypes #-}

module Moe.Infra.Rss.Source
  ( RssSource (..),
    SomeRssSource (..),
    Nyaa,
    AcgRip,
    Mikan,
    StandardRss,
    selectRssSource,
    getBaseUrl,
    generateSearchUrl,
    mikanIdToRssUrl,
    getChildText,
    extractInfoHashFromUrl,
    extractInfoHashFromMagnet,
    extractInfoHash,
  )
where

import Data.Char (isHexDigit)
import Data.Text qualified as T
import Moe.Domain.Rss (parsePubDate)
import Moe.Infra.Rss.Types (RawItem (..))
import Moe.Prelude
import Network.HTTP.Types (urlEncode)
import Text.XML (Name (..))
import Text.XML.Cursor

class RssSource s where
  -- | Base URL of the RSS source site.
  baseUrlFor :: proxy s -> Maybe Text
  baseUrlFor _ = Nothing
  parseItemFor :: proxy s -> Cursor -> RawItem
  searchUrlFor :: proxy s -> Text -> Maybe Text
  searchUrlFor _ _ = Nothing

data SomeRssSource = forall s. (RssSource s) => SomeRssSource (Proxy s)

data Nyaa

data AcgRip

data Mikan

data StandardRss

selectRssSource :: Text -> SomeRssSource
selectRssSource url
  | "nyaa.si" `T.isInfixOf` url = SomeRssSource (Proxy @Nyaa)
  | "acg.rip" `T.isInfixOf` url = SomeRssSource (Proxy @AcgRip)
  | "mikanani.me" `T.isInfixOf` url = SomeRssSource (Proxy @Mikan)
  | otherwise = SomeRssSource (Proxy @StandardRss)

getBaseUrl :: SomeRssSource -> Maybe Text
getBaseUrl (SomeRssSource proxy) = baseUrlFor proxy

generateSearchUrl :: SomeRssSource -> Text -> Maybe Text
generateSearchUrl (SomeRssSource proxy) = searchUrlFor proxy

-- | Build a Mikan RSS feed URL from a bangumi ID.
mikanIdToRssUrl :: Word32 -> Text
mikanIdToRssUrl mid = "https://mikanani.me/RSS/Bangumi?bangumiId=" <> show mid

instance RssSource StandardRss where
  parseItemFor _ = parseStandardRssItem

instance RssSource AcgRip where
  baseUrlFor _ = Just "https://acg.rip"
  parseItemFor _ = parseStandardRssItem
  searchUrlFor p keyword = do
    base <- baseUrlFor p
    Just $ base <> "/.xml?term=" <> encodeUrlParam keyword

parseStandardRssItem :: Cursor -> RawItem
parseStandardRssItem cursor =
  let url = getEnclosureUrl cursor <|> getChildText "link" cursor
   in RawItem
        { title = getChildText "title" cursor,
          pubDate = getChildText "pubDate" cursor >>= parsePubDate,
          torrentUrl = url,
          infoHash = url >>= extractInfoHash
        }

instance RssSource Mikan where
  baseUrlFor _ = Just "https://mikanani.me"
  parseItemFor _ cursor =
    let url = getEnclosureUrl cursor
     in RawItem
          { title = getChildText "title" cursor,
            pubDate = getMikanTorrentText "pubDate" cursor >>= parsePubDate,
            torrentUrl = url,
            infoHash = url >>= extractInfoHash
          }
   where
    getMikanTorrentText :: Text -> Cursor -> Maybe Text
    getMikanTorrentText localName c =
      case c $/ element mikanTorrentName &/ element (mikanName localName) &/ content of
        [] -> Nothing
        (t : _) -> Just t

    mikanNs :: Maybe Text
    mikanNs = Just "https://mikanani.me/0.1/"

    mikanName :: Text -> Name
    mikanName n = Name n mikanNs Nothing

    mikanTorrentName :: Name
    mikanTorrentName = mikanName "torrent"

instance RssSource Nyaa where
  baseUrlFor _ = Just "https://nyaa.si"
  parseItemFor _ cursor =
    let url = getEnclosureUrl cursor <|> getChildText "link" cursor
     in RawItem
          { title = getChildText "title" cursor,
            pubDate = getChildText "pubDate" cursor >>= parsePubDate,
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

  searchUrlFor p keyword = do
    base <- baseUrlFor p
    Just $ base <> "/?page=rss&q=" <> encodeUrlParam keyword

getChildText :: Text -> Cursor -> Maybe Text
getChildText name cursor =
  case cursor $/ element (textToName name) &/ content of
    [] -> Nothing
    (t : _) -> Just t
 where
  textToName :: Text -> Name
  textToName t = Name t Nothing Nothing

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
