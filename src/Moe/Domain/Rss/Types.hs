-- | RSS domain types
module Moe.Domain.Rss.Types
  ( PubDate (..),
    parsePubDate,
  )
where

import Data.Aeson (ToJSON)
import Data.OpenApi (ToSchema)
import Data.Time (UTCTime, defaultTimeLocale, parseTimeM)
import Moe.Prelude

-- | Publication date of an RSS item
newtype PubDate = PubDate {unPubDate :: UTCTime}
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (ToJSON, ToSchema)

-- | Parse a pub date string in various formats
parsePubDate :: Text -> Maybe PubDate
parsePubDate txt =
  let str = toString txt
   in PubDate
        <$> ( parseRfc822 str
                <|> parseIso8601 str
                <|> parseSimple str
            )
  where
    parseRfc822 = parseTimeM True defaultTimeLocale "%a, %d %b %Y %H:%M:%S %z"
    parseIso8601 = parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%z"
    parseSimple = parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S"
