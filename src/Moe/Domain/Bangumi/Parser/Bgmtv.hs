module Moe.Domain.Bangumi.Parser.Bgmtv
  ( BgmtvParsedTitle (..),
    parseBgmtvTitle,
  )
where

import Control.Applicative ((<|>))
import Data.Bifunctor (bimap)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word32)
import GHC.Generics (Generic)
import Moe.Domain.Bangumi.Parser.Internal.Pattern

data BgmtvParsedTitle = BgmtvParsedTitle
  { titleChs :: Text,
    titleJap :: Text,
    seasonNumber :: Maybe Word32
  }
  deriving stock (Eq, Show, Generic)

parseBgmtvTitle :: (Text, Text) -> BgmtvParsedTitle
parseBgmtvTitle (name, nameCn) =
  let (titleJap, seasonFromName) = parseTitle name
      (titleChs, seasonFromNameCn) = parseTitle nameCn
   in BgmtvParsedTitle
        { titleChs = titleChs,
          titleJap = titleJap,
          seasonNumber = seasonFromNameCn <|> seasonFromName
        }

parseTitle :: Text -> (Text, Maybe Word32)
parseTitle input =
  maybe
    (T.strip input, Nothing)
    (bimap T.strip extractSeasonNumber)
    (findPatternWithPosition seasonPattern input)

extractSeasonNumber :: Text -> Maybe Word32
extractSeasonNumber t =
  case extractNumber t of
    Just n -> Just n
    Nothing -> chineseToNumber t
