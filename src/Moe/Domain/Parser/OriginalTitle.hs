module Moe.Domain.Parser.OriginalTitle
  ( ParsedTitle (..),
    parseOriginalTitle,
  )
where

import Data.Text qualified as T
import Moe.Domain.Bangumi (SeasonNumber (..))
import Moe.Domain.Parser.Internal.Pattern
import Moe.Prelude

-- | Result of parsing an original title pair (Japanese + Chinese).
data ParsedTitle = ParsedTitle
  { titleChs :: Text,
    titleJap :: Text,
    season :: Maybe SeasonNumber
  }
  deriving stock (Eq, Show, Generic)

-- | Parse a (Japanese, Chinese) title pair, extracting season number
-- and cleaning season markers from titles.
-- Prefers season number from the Chinese title over the Japanese one.
parseOriginalTitle :: (Text, Text) -> ParsedTitle
parseOriginalTitle (name, nameCn) =
  let (titleJap, seasonFromName) = parseTitle name
      (titleChs, seasonFromNameCn) = parseTitle nameCn
   in ParsedTitle
        { titleChs = titleChs,
          titleJap = titleJap,
          season = seasonFromNameCn <|> seasonFromName
        }

parseTitle :: Text -> (Text, Maybe SeasonNumber)
parseTitle input =
  maybe
    (T.strip input, Nothing)
    (bimap T.strip extractSeasonNumber)
    (findPatternWithPosition seasonPattern input)

extractSeasonNumber :: Text -> Maybe SeasonNumber
extractSeasonNumber t =
  SeasonNumber . fromIntegral <$> case extractNumber t of
    Just n -> Just n
    Nothing -> chineseToNumber t
