module Moe.Domain.Parser.RssTitle
  ( RssTitleInfo (..),
    parseRssTitle,
  )
where

import Data.Text qualified as T
import Moe.Domain.Episode (EpisodeNumber (..))
import Moe.Domain.Shared.Group (Group, GroupName, splitGroupNames)
import Moe.Domain.Shared.Subtitle (Subtitle (..), SubtitleList)
import Moe.Domain.Parser.Internal.Pattern
import Moe.Prelude

data RssTitleInfo = RssTitleInfo
  { episode :: Maybe EpisodeNumber,
    group :: [GroupName],
    resolution :: Maybe Text,
    subtitleList :: SubtitleList
  }
  deriving stock (Eq, Show)

parseRssTitle :: [Group] -> Text -> RssTitleInfo
parseRssTitle groups rawInput =
  let processed = preProcess rawInput
      (rawGrp, grps) = extractSubtitleGroup groups processed
      withoutGroup = prefixProcess processed rawGrp
      episodeNum = extractEpisodeNumber withoutGroup
      (subs, res) = findTags processed
   in RssTitleInfo
        { episode = episodeNum,
          group = grps,
          resolution = res,
          subtitleList = subs
        }

preProcess :: Text -> Text
preProcess =
  removeFileExtension
    . normalizeBrackets
    . T.strip

removeFileExtension :: Text -> Text
removeFileExtension input =
  foldr tryRemove input [".mp4", ".mkv", ".avi", ".rmvb", ".wmv", ".flv"]
  where
    tryRemove ext t =
      if ext `T.isSuffixOf` T.toLower t
        then T.dropEnd (T.length ext) t
        else t

normalizeBrackets :: Text -> Text
normalizeBrackets =
  T.replace "【" "[" . T.replace "】" "]" . T.replace "（" "(" . T.replace "）" ")"

-- | Extract subtitle group, returning both raw text (for prefix removal) and split group names
extractSubtitleGroup :: [Group] -> Text -> (Maybe Text, [GroupName])
extractSubtitleGroup groups input =
  case findPattern groupPattern input of
    Just txt | not (T.null txt) ->
      let raw = stripBrackets txt
       in (Just raw, splitGroupNames groups raw)
    _ -> (Nothing, [])
  where
    stripBrackets t = fromMaybe t (T.stripPrefix "[" t >>= T.stripSuffix "]")

prefixProcess :: Text -> Maybe Text -> Text
prefixProcess input Nothing = input
prefixProcess input (Just grp) = T.strip $ T.replace ("[" <> grp <> "]") "" input

extractEpisodeNumber :: Text -> Maybe EpisodeNumber
extractEpisodeNumber input =
  let patterns =
        [ mkPattern "第(\\d+)[话話集]",
          mkPattern " - (\\d{1,3})($|[^\\d])",
          mkPattern " -(\\d{1,3})($|[^\\d])",
          mkPattern "- (\\d{1,3})($|[^\\d])",
          mkPattern "\\[(\\d{1,3})(\\.END|[vV]\\d)?\\]",
          mkPattern "\\[(\\d{1,3})(END|完)\\]",
          mkPattern "\\[(\\d{2})\\]",
          mkPattern "[Ee][Pp]?(\\d+)",
          mkPattern " (\\d{1,3})(完)?( |\\[|$)"
        ]
   in EpisodeNumber <$> listToMaybe (mapMaybe tryExtract patterns)
  where
    tryExtract pat = findPattern pat input >>= extractNumber

findTags :: Text -> ([Subtitle], Maybe Text)
findTags input = (findSubtitles input, findResolution input)

findSubtitles :: Text -> [Subtitle]
findSubtitles input = filter (hasPattern input) [CHS, CHT, JPN, ENG]
  where
    hasPattern t lang =
      let pat = case lang of
            CHS -> subChsPattern
            CHT -> subChtPattern
            JPN -> subJpnPattern
            ENG -> subEngPattern
       in matchPattern pat t

findResolution :: Text -> Maybe Text
findResolution input
  | matchPattern resolution2160Pattern input = Just "2160P"
  | matchPattern resolution1080Pattern input = Just "1080P"
  | matchPattern resolution720Pattern input = Just "720P"
  | otherwise = Nothing
