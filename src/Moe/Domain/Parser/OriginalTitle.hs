module Moe.Domain.Parser.OriginalTitle
  ( ParsedTitle (..),
    parseOriginalTitle,
  )
where

import Data.Text qualified as T
import Moe.Domain.Bangumi (SeasonIndex (..))
import Moe.Domain.Parser.Internal.Pattern
import Moe.Prelude

-- | Result of parsing an original title pair (Japanese + Chinese).
data ParsedTitle = ParsedTitle
  { titleChs :: Text,
    titleJap :: Text,
    season :: Maybe SeasonIndex
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

parseTitle :: Text -> (Text, Maybe SeasonIndex)
parseTitle input =
  maybe
    (T.strip input, Nothing)
    (bimap T.strip extractSeasonIndex)
    (findPatternWithPosition seasonPattern input)

extractSeasonIndex :: Text -> Maybe SeasonIndex
extractSeasonIndex t =
  SeasonIndex . fromIntegral <$> case extractNumber t of
    Just n -> Just n
    Nothing -> chineseToNumber t
