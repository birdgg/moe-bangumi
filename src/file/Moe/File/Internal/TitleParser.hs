module Moe.File.Internal.TitleParser
  ( parseTitle,
    extractGroup,
    extractEpisode,
    extractSubtitles,
  )
where

import Data.Char (isDigit)
import Data.Text qualified as T
import Moe.File.Internal.Group (normalizeGroup)
import Moe.File.Internal.Parser.Bracket (extractBracket)
import Moe.File.Internal.Parser.EpisodePattern (episodePatterns)
import Moe.File.Internal.Parser.Types (SubscriptionTitleResult (..))
import Moe.File.Types (SubtitleLang (..))
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
      unwords $ filter (not . isTechnicalSpec) $ words t

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
