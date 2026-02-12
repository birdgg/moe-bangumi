-- | RSS domain types
module Moe.Domain.Rss
  ( PubDate (..),
    parsePubDate,
    TorrentUrl,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToSchema)
import Data.Time (UTCTime, defaultTimeLocale, parseTimeM)
import Effectful.Sqlite (FromField, ToField)
import Moe.Prelude

-- | Publication date of an RSS item
newtype PubDate = PubDate {unPubDate :: UTCTime}
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (FromJSON, ToJSON, ToSchema, FromField, ToField)

-- | URL for a torrent resource.
type TorrentUrl = Text

-- | Parse a pub date string in various formats
parsePubDate :: Text -> Maybe PubDate
parsePubDate txt =
  let str = toString txt
   in PubDate
        <$> ( parseRfc822 str
                <|> parseIso8601 str
                <|> parseIso8601Frac str
                <|> parseIso8601NoTz str
                <|> parseSimple str
            )
  where
    parseRfc822 = parseTimeM True defaultTimeLocale "%a, %d %b %Y %H:%M:%S %z"
    parseIso8601 = parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%z"
    parseIso8601Frac = parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Q%z"
    parseIso8601NoTz = parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Q"
    parseSimple = parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S"
