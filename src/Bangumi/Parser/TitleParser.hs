module Bangumi.Parser.TitleParser
  ( parseTitle,
    extractGroup,
    extractEpisode,
    extractSubtitles,
  )
where

import Bangumi.Internal.Group (normalizeGroup)
import Bangumi.Internal.Subtitle (SubtitleLang (..))
import Bangumi.Parser.Internal.Bracket (extractBracket)
import Bangumi.Parser.Internal.EpisodePattern (episodePatterns)
import Bangumi.Parser.Types (SubscriptionTitleResult (..))
import Data.Char (isDigit)
import Data.Text qualified as T
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
