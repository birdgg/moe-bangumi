module Moe.Domain.Internal.File.TitleParser
  ( parseTitle,
    extractGroup,
    extractEpisode,
    extractSubtitles,
  )
where

import Data.Char (isDigit)
import Data.List (sort)
import Data.Maybe (mapMaybe, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Moe.Domain.File.Types (SubtitleLang (..))
import Moe.Domain.Internal.File.Group (normalizeGroup)
import Moe.Domain.Internal.File.Parser.Bracket (extractBracket)
import Moe.Domain.Internal.File.Parser.EpisodePattern (episodePatterns)
import Moe.Domain.Internal.File.Parser.Types (SubscriptionTitleResult (..))
import Text.Regex.TDFA ((=~))

parseTitle :: Text -> Maybe SubscriptionTitleResult
parseTitle input = do
  let processed = preprocess input
  ep <- extractEpisode processed
  pure
    SubscriptionTitleResult
      { episode = ep,
        subtitleGroup = normalizeGroup <$> extractGroup processed,
        subtitles = extractSubtitles processed
      }

preprocess :: Text -> Text
preprocess =
  removeTechnicalSpecs
    . T.replace "【" "["
    . T.replace "】" "]"
  where
    removeTechnicalSpecs t =
      T.unwords $ filter (not . isTechnicalSpec) $ T.words t

    isTechnicalSpec word =
      let lower = T.toLower word
       in any (`T.isSuffixOf` lower) ["fps", "bit", "khz", "hz"]
            && T.any isDigit word

extractGroup :: Text -> Maybe Text
extractGroup = extractBracket

extractEpisode :: Text -> Maybe Int
extractEpisode input = listToMaybe $ mapMaybe tryPattern episodePatterns
  where
    tryPattern (pat, extractor) =
      case input =~ pat :: (Text, Text, Text) of
        (_, match, _)
          | not (T.null match) -> extractor match
          | otherwise -> Nothing

extractSubtitles :: Text -> [SubtitleLang]
extractSubtitles t =
  ordNub $
    sort $
      concat
        [ [CHT | matchesCht],
          [CHS | matchesChs],
          [JPN | matchesJpn],
          [ENG | matchesEng]
        ]
  where
    matchesChs = t =~ ("简|CHS|SC|GB" :: Text)
    matchesCht = t =~ ("繁|CHT|BIG5" :: Text)
    matchesJpn = t =~ ("日|JP|JPSC" :: Text)
    matchesEng = t =~ ("ENG|英语|英文" :: Text)

ordNub :: (Ord a) => [a] -> [a]
ordNub = go mempty
  where
    go _ [] = []
    go seen (x : xs)
      | x `elem` seen = go seen xs
      | otherwise = x : go (x : seen) xs
