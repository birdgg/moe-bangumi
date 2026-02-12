-- | Parsers for collection torrent directory and file names.
--
-- Extracts search keywords, group names, season numbers, and episode numbers
-- from VCB-Studio style directory names and video file names.
module Moe.Domain.Parser.Collection
  ( ParsedDirName (..),
    parseCollectionDirName,
    ParsedInfo (..),
    parseInfo,
    extractSeason,
    extractEpisode,
    extractGroup,
    ParsedSpContent (..),
    parseSpContent,
  )
where

import Data.Char (isDigit)
import Data.Text qualified as T
import Moe.Domain.Bangumi (SeasonNumber (..))
import Moe.Domain.Episode (EpisodeNumber (..))
import Moe.Domain.File (BangumiContent (..), ExtraIndex (..))
import Moe.Domain.Parser.Internal.Pattern
import Moe.Prelude

-- ---------------------------------------------------------------------------
-- File name parsing
-- ---------------------------------------------------------------------------

-- | Result of parsing a video file name.
data ParsedInfo = ParsedInfo
  { title :: Text,
    season :: Maybe SeasonNumber,
    episodeNumber :: Maybe EpisodeNumber,
    group :: Maybe Text
  }
  deriving stock (Eq, Show)

-- | Parse a video file name to extract title, season, episode, and group.
parseInfo :: Text -> ParsedInfo
parseInfo input =
  ParsedInfo
    { title = input,
      season = extractSeason input,
      episodeNumber = extractEpisode input,
      group = extractGroup input
    }

-- | Extract season number from text.
extractSeason :: Text -> Maybe SeasonNumber
extractSeason input = do
  matched <- findPattern seasonPattern input
  SeasonNumber . fromIntegral <$> extractNumber matched

-- | Extract episode number from a bracket tag like @[29]@, @[01]@.
--
-- Only matches pure-digit brackets to avoid @[Ma10p_1080p]@ or @[x265_flac_aac]@.
extractEpisode :: Text -> Maybe EpisodeNumber
extractEpisode input = do
  matched <- findPattern bracketEpisodePattern input
  let digits = T.filter isDigit matched
  EpisodeNumber <$> readMaybe (toString digits)

-- | Pattern for episode number in brackets: @[29]@, @[01]@.
bracketEpisodePattern :: Pattern
bracketEpisodePattern = mkPattern "\\[\\d+\\]"

-- | Extract group name from text (e.g. @[VCB-Studio]@ -> @VCB-Studio@).
extractGroup :: Text -> Maybe Text
extractGroup input = do
  txt <- findPattern groupPattern input
  if T.null txt then Nothing else Just (stripBrackets txt)

-- ---------------------------------------------------------------------------
-- Directory name parsing
-- ---------------------------------------------------------------------------

-- | Result of parsing a collection directory name.
data ParsedDirName = ParsedDirName
  { keyword :: Text,
    group :: Maybe Text,
    season :: Maybe SeasonNumber
  }
  deriving stock (Eq, Show)

-- | Parse a collection directory name to extract search keyword, group, and season.
parseCollectionDirName :: Text -> ParsedDirName
parseCollectionDirName input =
  let grp = extractGroup input
      season = extractSeason input
      keyword = extractKeyword grp input
   in ParsedDirName {keyword, group = grp, season}

-- | Extract the search keyword by stripping group tags, quality tags, and season suffixes.
extractKeyword :: Maybe Text -> Text -> Text
extractKeyword grp =
  T.strip
    . stripBracketTags
    . stripGroupPrefix grp

-- | Remove leading group tag: @[VCB-Studio] Title@ -> @Title@.
stripGroupPrefix :: Maybe Text -> Text -> Text
stripGroupPrefix Nothing t = t
stripGroupPrefix (Just grp) t =
  maybe t T.strip (T.stripPrefix ("[" <> grp <> "]") (T.strip t))

-- | Remove all bracket tags: @[Ma10p_1080p]@, @[x265_flac]@, @[BDRip]@ etc.
stripBracketTags :: Text -> Text
stripBracketTags = T.strip . go
  where
    go t = case findPatternWithPosition bracketTagPattern t of
      Just (before, _matched) ->
        let afterMatch = T.drop (T.length before + T.length _matched) t
         in go (before <> afterMatch)
      Nothing -> t

-- | Pattern for bracket tags containing technical specs.
bracketTagPattern :: Pattern
bracketTagPattern =
  mkPattern
    "\\[[^\\]]*(?:10p|10bit|1080p|720p|2160p|4K|x264|x265|flac|aac|hevc|bdrip|ma10p|hi10p|yuv|AVC|HEVC)[^\\]]*\\]"

-- ---------------------------------------------------------------------------
-- SP content parsing
-- ---------------------------------------------------------------------------

-- | Result of parsing SP (special) content from a filename.
data ParsedSpContent
  = -- | Extras/trailers content (NCOP, NCED, CM, PV, Menu, Trailer, Preview).
    ExtraContent BangumiContent
  | -- | Special episode, placed in Season 00.
    SpecialEpisode EpisodeNumber
  deriving stock (Eq, Show)

-- | Parse SP content type from a video filename.
--
-- >>> parseSpContent "[VCB-Studio] Anime [NCOP01][1080p].mkv"
-- Just (ExtraContent (NCOP (Just (ExtraIndex 1))))
-- >>> parseSpContent "[VCB-Studio] Anime [SP01_12][1080p].mkv"
-- Just (SpecialEpisode (EpisodeNumber 112))
-- >>> parseSpContent "[VCB-Studio] Anime [26][1080p].mkv"
-- Nothing
parseSpContent :: Text -> Maybe ParsedSpContent
parseSpContent input =
  asum
    [ matchExtraWithEp NCOP ncopPat "NCOP" input,
      matchExtraWithEp NCED ncedPat "NCED" input,
      matchExtraWith CM cmPat "CM" input,
      matchExtraWith Menu menuPat "Menu" input,
      matchPv input,
      ExtraContent (Trailer Nothing) <$ findPattern trailerPat input,
      matchPreview input,
      matchSp input
    ]

-- | Match an extra content type with optional index and episode number (NCOP, NCED).
matchExtraWithEp ::
  (Maybe ExtraIndex -> Maybe EpisodeNumber -> BangumiContent) ->
  Pattern ->
  Text ->
  Text ->
  Maybe ParsedSpContent
matchExtraWithEp ctor pat prefix input = do
  matched <- findPattern pat input
  let inner = stripBrackets matched
      rest = T.drop (T.length prefix) inner
      idx = parseLeadingIndex rest
      ep = parseEpSuffix rest
  pure $ ExtraContent (ctor idx ep)

-- | Match an extra content type with optional index (CM, Menu).
matchExtraWith ::
  (Maybe ExtraIndex -> BangumiContent) ->
  Pattern ->
  Text ->
  Text ->
  Maybe ParsedSpContent
matchExtraWith ctor pat prefix input = do
  matched <- findPattern pat input
  let inner = stripBrackets matched
      idx = parseLeadingIndex (T.drop (T.length prefix) inner)
  pure $ ExtraContent (ctor idx)

-- | Parse @_EP\d+@ suffix as an EpisodeNumber.
parseEpSuffix :: Text -> Maybe EpisodeNumber
parseEpSuffix t = case T.breakOn "_EP" t of
  (_, rest) | not (T.null rest) ->
    let digits = T.takeWhile isDigit (T.drop 3 rest)
     in EpisodeNumber <$> readMaybe (toString digits)
  _ -> Nothing

-- | Match PV bracket tag, defaulting index to 1.
matchPv :: Text -> Maybe ParsedSpContent
matchPv input = do
  matched <- findPattern pvPat input
  let inner = stripBrackets matched
      n = fromMaybe 1 $ parseLeadingIndex (T.drop 2 inner)
  pure $ ExtraContent (PV n)

-- | Match Preview bracket tag, extracting optional episode number.
--
-- >>> matchPreview "[Web Preview 27][1080p].mkv"
-- Just (ExtraContent (Preview (Just (EpisodeNumber 27))))
matchPreview :: Text -> Maybe ParsedSpContent
matchPreview input = do
  matched <- findPattern previewPat input
  let inner = stripBrackets matched
      digits = T.takeWhileEnd isDigit inner
      ep = EpisodeNumber <$> readMaybe (toString digits)
  pure $ ExtraContent (Preview ep)

-- | Match SP bracket tag for special episodes.
matchSp :: Text -> Maybe ParsedSpContent
matchSp input = do
  matched <- findPattern spPat input
  let inner = stripBrackets matched
      rest = T.drop 2 inner -- strip "SP"
  pure $ SpecialEpisode $ case T.splitOn "_" rest of
    [vol, track]
      | Just v <- readDigits vol,
        Just t <- readDigits track ->
          EpisodeNumber (v * 100 + t)
    _ -> EpisodeNumber (fromMaybe 1 (readDigits rest))

-- | Parse leading digits as an ExtraIndex.
parseLeadingIndex :: Text -> Maybe ExtraIndex
parseLeadingIndex t =
  let digits = T.takeWhile isDigit t
   in if T.null digits
        then Nothing
        else ExtraIndex <$> readMaybe (toString digits)

-- | Read all digits from text as a Word32.
readDigits :: Text -> Maybe Word32
readDigits t =
  let digits = T.takeWhile isDigit t
   in if T.null digits then Nothing else readMaybe (toString digits)

-- | Strip surrounding brackets from text.
stripBrackets :: Text -> Text
stripBrackets t = fromMaybe t (T.stripPrefix "[" t >>= T.stripSuffix "]")

ncopPat :: Pattern
ncopPat = mkPattern "\\[NCOP\\d*[^\\]]*\\]"

ncedPat :: Pattern
ncedPat = mkPattern "\\[NCED\\d*[^\\]]*\\]"

cmPat :: Pattern
cmPat = mkPattern "\\[CM\\d*[^\\]]*\\]"

menuPat :: Pattern
menuPat = mkPattern "\\[Menu\\d*[^\\]]*\\]"

pvPat :: Pattern
pvPat = mkPattern "\\[PV\\d*\\]"

trailerPat :: Pattern
trailerPat = mkPattern "\\[Trailer\\]"

previewPat :: Pattern
previewPat = mkPattern "\\[(?:Web )?Preview[^\\]]*\\]"

spPat :: Pattern
spPat = mkPattern "\\[SP\\d*[^\\]]*\\]"
