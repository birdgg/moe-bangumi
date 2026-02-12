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
    extrasDir,
    trailersDir,
    isVideoExt,
  )
where

import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Time.Calendar (Year)
import Moe.Domain.Bangumi (Bangumi (..), SeasonNumber (..), TmdbId (..), extractYear)
import Moe.Domain.Episode (EpisodeNumber (..))
import Moe.Domain.Episode qualified as Ep
import Moe.Domain.Shared.Group (GroupName (..))
import Moe.Domain.Shared.Subtitle (Subtitle (..), SubtitleList)
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
  Movie movieYear -> moviePath movieYear
  MovieSub movieYear _ -> moviePath movieYear
  content -> showBaseName file.bangumi </> contentDir content
  where
    moviePath y = toString $ nameWithYear file.bangumi.title y
    s = fromMaybe (SeasonNumber 1) file.bangumi.season
    contentDir = \case
      Episode {} -> seasonDir s
      EpisodeSub {} -> seasonDir s
      NCOP {} -> seasonDir s </> extrasDir
      NCED {} -> seasonDir s </> extrasDir
      Menu {} -> seasonDir s </> extrasDir
      _ -> seasonDir s </> trailersDir

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
      EpisodeSub _ sub -> "." <> toText sub <> "." <> file.ext
      MovieSub _ sub -> "." <> toText sub <> "." <> file.ext
      _ -> "." <> file.ext

-- | Generate complete path including directory and file name.
--
-- >>> generateFullPath (BangumiFile meta (Episode 1 1) "mkv")
-- "Frieren (2023)/Season 01/Frieren - S01E01.mkv"
-- >>> generateFullPath (BangumiFile meta (NCOP Nothing) "mkv")
-- "Frieren (2023)/extras/NCOP.mkv"
generateFullPath :: BangumiFile -> FilePath
generateFullPath file = generatePath file </> generateFileName file

sanitizeName :: Text -> Text
sanitizeName = T.map replaceChar . T.filter (`notElem` forbiddenChars)
  where
    forbiddenChars :: [Char]
    forbiddenChars = "<>|?*\""
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

-- | Media server extras directory for credit-less content (NCOP, NCED, Menu).
extrasDir :: FilePath
extrasDir = "extras"

-- | Media server trailers directory for promotional content (PV, Preview, Trailer, CM).
trailersDir :: FilePath
trailersDir = "trailers"

contentBaseName :: BangumiMeta -> BangumiContent -> Text
contentBaseName meta = \case
  Episode ep -> epBase ep
  EpisodeSub ep _ -> epBase ep
  NCOP idx mEp -> extraBaseWithEp "NCOP" idx mEp
  NCED idx mEp -> extraBaseWithEp "NCED" idx mEp
  Menu idx -> extraBase "Menu" idx
  PV (ExtraIndex i) -> "PV" <> show i
  Preview mEp -> "Preview" <> foldMap (\ep -> "_EP" <> toText ep) mEp
  Trailer mEp -> "Trailer" <> foldMap (\ep -> "_EP" <> toText ep) mEp
  CM idx -> extraBase "CM" idx
  Movie y -> withSuffixes $ nameWithYear meta.title y
  MovieSub y _ -> withSuffixes $ nameWithYear meta.title y
  where
    s = fromMaybe (SeasonNumber 1) meta.season
    withSuffixes base =
      base <> foldMap (\g -> " [" <> toText g <> "]") meta.group
        <> if meta.isBDrip then "[BDRip]" else ""
    epBase ep = withSuffixes $ sanitizeName meta.title <> " - S" <> toText s <> "E" <> toText ep
    extraBase label = \case
      Nothing -> label
      Just (ExtraIndex i) -> label <> show i
    extraBaseWithEp label idx mEp =
      extraBase label idx <> foldMap (\ep -> "_EP" <> toText ep) mEp

-- | Check if a file extension is a known video format.
isVideoExt :: Text -> Bool
isVideoExt ext = T.toLower ext `Set.member` videoExtensions

videoExtensions :: Set Text
videoExtensions = Set.fromList ["mkv", "mka", "mp4", "avi", "wmv", "flv", "webm", "rmvb", "ts", "m2ts"]
