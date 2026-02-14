module Moe.Domain.File
  ( -- * Types
    Subtitle (..),
    SubtitleList,
    EpisodeNumber (..),
    ExtraIndex (..),
    Year,
    TmdbId (..),
    SeasonNumber (..),
    GroupName (..),
    BangumiContent (..),
    BangumiMeta (..),
    BangumiFile (..),
    toBangumiMeta,
    toBangumiFile,

    -- * Naming
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
  )
where

import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Time.Calendar (Year)
import Moe.Domain.Bangumi (Bangumi (..), SeasonNumber (..), TmdbId (..), extractYear)
import Moe.Domain.Episode (EpisodeNumber (..))
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

-- | Content type of a bangumi file.
data BangumiContent
  = Episode EpisodeNumber
  | EpisodeSub EpisodeNumber Subtitle
  | NCOP (Maybe ExtraIndex) (Maybe EpisodeNumber)
  | NCED (Maybe ExtraIndex) (Maybe EpisodeNumber)
  | Menu (Maybe ExtraIndex)
  | PV ExtraIndex
  | Preview (Maybe EpisodeNumber)
  | Trailer (Maybe EpisodeNumber)
  | CM (Maybe ExtraIndex)
  | Movie Year
  | MovieSub Year Subtitle
  deriving stock (Eq, Show)

-- | Lightweight metadata for file naming on media servers.
data BangumiMeta = BangumiMeta
  { title :: Text,
    year :: Maybe Year,
    tmdbId :: Maybe TmdbId,
    season :: Maybe SeasonNumber,
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
      year = Just (extractYear b.airDate),
      tmdbId = b.tmdbId,
      season = b.season,
      group = [],
      isBDrip = False
    }

-- | Convert Bangumi and Episode to BangumiFile for media server storage.
toBangumiFile :: Bangumi -> Ep.Episode -> BangumiFile
toBangumiFile b ep =
  let meta = toBangumiMeta b
      content = Episode ep.episodeNumber
   in BangumiFile {bangumi = meta {group = ep.group}, content, ext = "mkv"}

-- ---------------------------------------------------------------------
-- Naming
-- ---------------------------------------------------------------------

-- | Generate directory path for a bangumi file.
--
-- >>> generatePath (BangumiFile meta (Episode 1 1) "mkv")
-- "Frieren (2023)/Season 01"
-- >>> generatePath (BangumiFile meta (NCOP Nothing) "mkv")
-- "Frieren (2023)/extras"
-- >>> generatePath (BangumiFile meta (Movie 2025) "mkv")
-- "Frieren Movie (2025)"
generatePath :: BangumiFile -> FilePath
generatePath file = case file.content of
  Movie y -> toString $ nameWithYear file.bangumi.title y
  MovieSub y _ -> toString $ nameWithYear file.bangumi.title y
  Episode {} -> showBaseName file.bangumi </> seasonDir s
  EpisodeSub {} -> showBaseName file.bangumi </> seasonDir s
  NCOP {} -> showBaseName file.bangumi </> withSeason featurettesDir
  NCED {} -> showBaseName file.bangumi </> withSeason featurettesDir
  PV {} -> showBaseName file.bangumi </> withSeason trailersDir
  Preview {} -> showBaseName file.bangumi </> withSeason trailersDir
  Trailer {} -> showBaseName file.bangumi </> withSeason trailersDir
  _ -> showBaseName file.bangumi </> withSeason otherDir
  where
    s = fromMaybe (SeasonNumber 1) file.bangumi.season
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
-- >>> generateFileName (BangumiFile meta (EpisodeSub 1 1 CHS) "ass")
-- "Frieren - S01E01.chs.ass"
generateFileName :: BangumiFile -> FilePath
generateFileName file = generateBaseName file <> toString extension
  where
    extension = case file.content of
      EpisodeSub _ sub -> "." <> toMediaCode sub <> "." <> file.ext
      MovieSub _ sub -> "." <> toMediaCode sub <> "." <> file.ext
      _ -> "." <> file.ext

-- | Generate complete path including directory and file name.
--
-- >>> generateFullPath (BangumiFile meta (Episode 1 1) "mkv")
-- "Frieren (2023)/Season 01/Frieren - S01E01.mkv"
-- >>> generateFullPath (BangumiFile meta (NCOP Nothing Nothing) "mkv")
-- "Frieren (2023)/Season 01/Featurettes/NCOP.mkv"
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
showBaseName b = toString $ maybe (sanitizeName b.title) (nameWithYear b.title) b.year

nameWithYear :: Text -> Year -> Text
nameWithYear name y = sanitizeName name <> " (" <> toText y <> ")"

seasonDir :: SeasonNumber -> FilePath
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
contentBaseName meta = \case
  Episode ep -> epBase ep
  EpisodeSub ep _ -> epBase ep
  NCOP idx mEp -> extraWithEp "NCOP" idx mEp
  NCED idx mEp -> extraWithEp "NCED" idx mEp
  Menu idx -> extraIdx "Menu" idx
  PV (ExtraIndex i) -> "PV" <> show i
  Preview mEp -> "Preview" <> epSuffix mEp
  Trailer mEp -> "Trailer" <> epSuffix mEp
  CM idx -> extraIdx "CM" idx
  Movie y -> withSuffixes $ nameWithYear meta.title y
  MovieSub y _ -> withSuffixes $ nameWithYear meta.title y
  where
    s = fromMaybe (SeasonNumber 1) meta.season
    groupSuffix = foldMap (\g -> " [" <> toText g <> "]") meta.group
    bdripSuffix = if meta.isBDrip then "[BDRip]" else ""
    withSuffixes base = base <> groupSuffix <> bdripSuffix
    epBase ep = withSuffixes $ sanitizeName meta.title <> " - S" <> toText s <> "E" <> toText ep
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
