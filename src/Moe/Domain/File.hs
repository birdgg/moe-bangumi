module Moe.Domain.File
  ( -- * Types
    Subtitle (..),
    SubtitleList,
    EpisodeIndex (..),
    ExtraIndex (..),
    TmdbId (..),
    SeasonIndex (..),
    GroupName (..),
    ContentKind (..),
    BangumiContent (..),
    mkContent,
    mkContentSub,
    BangumiMeta (..),
    BangumiFile (..),
    toBangumiMeta,
    toBangumiFile,

    -- * Naming (forward: metadata -> file paths)
    generatePath,
    generateBaseName,
    generateFileName,
    generateFullPath,
    showBaseName,
    sanitizeName,
    seasonDir,
    featurettesDir,
    trailersDir,
    otherDir,
    isVideoExt,
    isSubtitleExt,

    -- * Parsing (reverse: file paths -> metadata)
    parseFolderName,
    parseSeasonDir,
    isBangumiDir,
  )
where

import Data.Char (isDigit)
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Time.Calendar (Year)
import Moe.Domain.Bangumi (Bangumi (..), SeasonIndex (..), TmdbId (..))
import Moe.Domain.Episode (EpisodeIndex (..))
import Moe.Domain.Episode qualified as Ep
import Moe.Domain.Shared.Group (GroupName (..))
import Moe.Domain.Shared.Subtitle (Subtitle (..), SubtitleList, toMediaCode)
import Moe.Prelude
import System.FilePath ((</>))

-- ---------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------

newtype ExtraIndex = ExtraIndex Word8
  deriving stock (Eq, Show)
  deriving newtype (Num)

-- | The kind of content in a bangumi file.
data ContentKind
  = Episode EpisodeIndex
  | Special EpisodeIndex
  | NCOP (Maybe ExtraIndex) (Maybe EpisodeIndex)
  | NCED (Maybe ExtraIndex) (Maybe EpisodeIndex)
  | Menu (Maybe ExtraIndex)
  | PV ExtraIndex
  | Preview (Maybe EpisodeIndex)
  | Trailer (Maybe EpisodeIndex)
  | CM (Maybe ExtraIndex)
  | Movie
  deriving stock (Eq, Show)

-- | Content type of a bangumi file, combining content kind with optional subtitle.
data BangumiContent = BangumiContent
  { kind :: ContentKind,
    subtitle :: Maybe Subtitle
  }
  deriving stock (Eq, Show)

-- | Create content without subtitle.
mkContent :: ContentKind -> BangumiContent
mkContent k = BangumiContent k Nothing

-- | Create content with subtitle.
mkContentSub :: ContentKind -> Subtitle -> BangumiContent
mkContentSub k s = BangumiContent k (Just s)

-- | Lightweight metadata for file naming on media servers.
data BangumiMeta = BangumiMeta
  { title :: Text,
    tmdbId :: Maybe TmdbId,
    season :: Maybe SeasonIndex,
    group :: [GroupName],
    isBDrip :: Bool
  }
  deriving stock (Eq, Show)

data BangumiFile = BangumiFile
  { bangumi :: BangumiMeta,
    content :: BangumiContent,
    ext :: Text
  }
  deriving stock (Eq, Show)

-- | Convert a Bangumi to lightweight BangumiMeta for file naming.
toBangumiMeta :: Bangumi -> BangumiMeta
toBangumiMeta b =
  BangumiMeta
    { title = b.titleChs,
      tmdbId = b.tmdbId,
      season = b.season,
      group = [],
      isBDrip = False
    }

-- | Convert Bangumi and Episode to BangumiFile for media server storage.
toBangumiFile :: Bangumi -> Ep.Episode -> BangumiFile
toBangumiFile b ep =
  let meta = toBangumiMeta b
      content = mkContent (Episode ep.episodeNumber)
   in BangumiFile {bangumi = meta {group = ep.group}, content, ext = "mkv"}

-- ---------------------------------------------------------------------
-- Naming
-- ---------------------------------------------------------------------

-- | Generate directory path for a bangumi file.
--
-- >>> generatePath (BangumiFile meta (Episode 1) "mkv")
-- "Frieren/Season 01"
-- >>> generatePath (BangumiFile meta (NCOP Nothing Nothing) "mkv")
-- "Frieren/Featurettes"
-- >>> generatePath (BangumiFile meta Movie "mkv")
-- "Frieren Movie"
generatePath :: BangumiFile -> FilePath
generatePath file = case file.content.kind of
  Movie -> movieDir
  Special {} -> base </> seasonDir (SeasonIndex 0)
  Episode {} -> base </> seasonDir s
  NCOP {} -> base </> withSeason featurettesDir
  NCED {} -> base </> withSeason featurettesDir
  PV {} -> base </> withSeason trailersDir
  Preview {} -> base </> withSeason trailersDir
  Trailer {} -> base </> withSeason trailersDir
  _ -> base </> withSeason otherDir
  where
    base = showBaseName file.bangumi
    s = fromMaybe (SeasonIndex 1) file.bangumi.season
    movieDir = toString $ sanitizeName file.bangumi.title
    withSeason dir = maybe dir (\sn -> seasonDir sn </> dir) file.bangumi.season

-- | Generate file base name without extension.
--
-- >>> generateBaseName (BangumiFile meta (Episode 1 1) "mkv")
-- "Frieren - S01E01"
-- >>> generateBaseName (BangumiFile meta (NCOP Nothing) "mkv")
-- "NCOP"
generateBaseName :: BangumiFile -> FilePath
generateBaseName file = toString $ contentBaseName file.bangumi file.content

-- | Generate file name with extension.
--
-- >>> generateFileName (BangumiFile meta (Episode 1 1) "mkv")
-- "Frieren - S01E01.mkv"
-- >>> generateFileName (BangumiFile meta (mkContentSub (Episode 1) CHS) "ass")
-- "Frieren - S01E01.chs.ass"
generateFileName :: BangumiFile -> FilePath
generateFileName file = generateBaseName file <> toString extension
  where
    subCode = foldMap (\sub -> toMediaCode sub <> ".") file.content.subtitle
    extension = "." <> subCode <> file.ext

-- | Generate complete path including directory and file name.
--
-- >>> generateFullPath (BangumiFile meta (Episode 1) "mkv")
-- "Frieren/Season 01/Frieren - S01E01.mkv"
-- >>> generateFullPath (BangumiFile meta (NCOP Nothing Nothing) "mkv")
-- "Frieren/Season 01/Featurettes/NCOP.mkv"
generateFullPath :: BangumiFile -> FilePath
generateFullPath file = generatePath file </> generateFileName file

-- | Characters forbidden in file names.
forbiddenChars :: [Char]
forbiddenChars = "<>|?*\""

sanitizeName :: Text -> Text
sanitizeName = T.map replaceChar . T.filter (`notElem` forbiddenChars)
  where
    replaceChar ':' = '-'
    replaceChar '/' = '-'
    replaceChar '\\' = '-'
    replaceChar c = c

showBaseName :: BangumiMeta -> FilePath
showBaseName b = toString (sanitizeName b.title)

seasonDir :: SeasonIndex -> FilePath
seasonDir s = toString $ "Season " <> toText s

-- | Plex extras directory for credit-less content (NCOP, NCED).
featurettesDir :: FilePath
featurettesDir = "Featurettes"

-- | Plex extras directory for promotional content (PV, Preview, Trailer).
trailersDir :: FilePath
trailersDir = "Trailers"

-- | Plex extras directory for miscellaneous content (CM, Menu).
otherDir :: FilePath
otherDir = "Other"

contentBaseName :: BangumiMeta -> BangumiContent -> Text
contentBaseName meta content = case content.kind of
  Episode ep -> epBase ep
  Special ep -> spBase ep
  NCOP idx mEp -> extraWithEp "NCOP" idx mEp
  NCED idx mEp -> extraWithEp "NCED" idx mEp
  Menu idx -> extraIdx "Menu" idx
  PV (ExtraIndex i) -> "PV" <> show i
  Preview mEp -> "Preview" <> epSuffix mEp
  Trailer mEp -> "Trailer" <> epSuffix mEp
  CM idx -> extraIdx "CM" idx
  Movie -> withSuffixes $ sanitizeName meta.title
  where
    s = fromMaybe (SeasonIndex 1) meta.season
    groupSuffix = foldMap (\g -> " [" <> toText g <> "]") meta.group
    bdripSuffix = if meta.isBDrip then "[BDRip]" else ""
    withSuffixes base = base <> groupSuffix <> bdripSuffix
    epBase ep = withSuffixes $ sanitizeName meta.title <> " - S" <> toText s <> "E" <> toText ep
    spBase ep = withSuffixes $ sanitizeName meta.title <> " - S00E" <> toText ep
    extraIdx label idx = label <> foldMap (\(ExtraIndex i) -> show i) idx
    epSuffix = foldMap (\ep -> "_EP" <> toText ep)
    extraWithEp label idx mEp = extraIdx label idx <> epSuffix mEp

-- | Check if a file extension is a known video format.
isVideoExt :: Text -> Bool
isVideoExt ext = T.toLower ext `Set.member` videoExtensions

videoExtensions :: Set Text
videoExtensions = Set.fromList ["mkv", "mka", "mp4", "avi", "wmv", "flv", "webm", "rmvb", "ts", "m2ts"]

-- | Check if a file extension is a known subtitle format.
isSubtitleExt :: Text -> Bool
isSubtitleExt ext = T.toLower ext `Set.member` subtitleExtensions

subtitleExtensions :: Set Text
subtitleExtensions = Set.fromList ["ass", "ssa", "srt", "sub", "sup", "idx"]

-- ---------------------------------------------------------------------
-- Parsing (reverse of naming)
-- ---------------------------------------------------------------------

-- | Parse a folder name like "Title (2023)" or "Title (2023) {tmdb-120089}"
-- into (title, Maybe year, Maybe TmdbId).
--
-- Reverse of 'nameWithYear'. Also handles Plex/Jellyfin agent hint format.
parseFolderName :: Text -> (Text, Maybe Year, Maybe TmdbId)
parseFolderName name =
  let (base, mTmdbId) = stripTmdbHint name
   in case T.stripSuffix ")" base of
        Just rest ->
          let yearText = T.takeWhileEnd isDigit rest
              beforeYear = T.dropEnd (T.length yearText) rest
           in case (readMaybe (toString yearText) :: Maybe Year, T.stripSuffix " (" beforeYear) of
                (Just y, Just title) | T.length yearText == 4 -> (T.strip title, Just y, mTmdbId)
                _ -> (T.strip base, Nothing, mTmdbId)
        Nothing -> (T.strip base, Nothing, mTmdbId)

-- | Strip a trailing "{tmdb-NNNNN}" hint from a folder name.
stripTmdbHint :: Text -> (Text, Maybe TmdbId)
stripTmdbHint t = case T.stripSuffix "}" t of
  Just rest -> case T.breakOnEnd "{tmdb-" rest of
    ("", _) -> (t, Nothing)
    (before, idText) ->
      let mId = TmdbId <$> readMaybe (toString idText)
       in case mId of
            Just tid -> (T.strip (T.dropEnd (T.length "{tmdb-") before), Just tid)
            Nothing -> (t, Nothing)
  Nothing -> (t, Nothing)

-- | Parse "Season 01" into SeasonIndex.
--
-- Reverse of 'seasonDir'.
parseSeasonDir :: Text -> Maybe SeasonIndex
parseSeasonDir name = case T.stripPrefix "Season " name of
  Just rest -> SeasonIndex <$> readMaybe (toString rest)
  Nothing -> Nothing

-- | Check if a directory name looks like a bangumi directory.
--
-- Rejects names starting with "." or "\@" (system/hidden directories).
isBangumiDir :: Text -> Bool
isBangumiDir name
  | T.null name = False
  | T.head name == '.' = False
  | T.head name == '@' = False
  | otherwise = True
