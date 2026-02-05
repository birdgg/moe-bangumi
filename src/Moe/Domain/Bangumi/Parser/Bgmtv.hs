module Moe.Domain.Bangumi.Parser.Bgmtv
  ( BgmtvParsedTitle (..),
    parseBgmtvTitle,
  )
where

import Data.Text qualified as T
import Moe.Domain.Bangumi.Parser.Internal.Pattern
import Moe.Domain.Bangumi.Types (SeasonNumber (..))
import Moe.Prelude

data BgmtvParsedTitle = BgmtvParsedTitle
  { titleChs :: Text,
    titleJap :: Text,
    season :: Maybe SeasonNumber
  }
  deriving stock (Eq, Show, Generic)

parseBgmtvTitle :: (Text, Text) -> BgmtvParsedTitle
parseBgmtvTitle (name, nameCn) =
  let (titleJap, seasonFromName) = parseTitle name
      (titleChs, seasonFromNameCn) = parseTitle nameCn
   in BgmtvParsedTitle
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
