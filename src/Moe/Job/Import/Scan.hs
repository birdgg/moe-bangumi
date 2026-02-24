-- | Scan filesystem for existing bangumi and import them into the database.
--
-- Reads the downloader save path, traverses bangumi directories,
-- resolves metadata via TMDB, and creates bangumi + tracking records
-- for directories not already tracked.
module Moe.Job.Import.Scan
  ( importExistingBangumi,
    ImportResult (..),
    ImportedEntry (..),
    SkippedEntry (..),
    SkipReason (..),
  )
where

import Control.Monad (foldM)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Effectful
import Effectful.FileSystem (doesDirectoryExist, listDirectory)
import Effectful.Log qualified as Log
import Effectful.Sqlite (SqliteTransaction, transact)
import Data.Time.Calendar (Year)
import Moe.Domain.Bangumi (Bangumi (..), BangumiKind (..), SeasonIndex (..))
import Moe.Domain.File (TmdbId (..), extractMaxEpisode, isBangumiDir, isVideoExt, parseFolderName, parseSeasonDir)
import Moe.Domain.Setting (DownloaderConfig (savePath), UserPreference (downloader))
import Moe.Domain.Shared.Entity (Entity (..), Id (..))
import Moe.Domain.Tracking qualified as Tracking
import Moe.Infra.Database.Bangumi qualified as BangumiDB
import Moe.Infra.Database.Tracking qualified as TrackingDB
import Moe.Infra.Metadata.Effect (Metadata, getTmdbMovieDetail, getTmdbTvDetail, searchTmdb)
import Network.Tmdb (MovieId (..), TvShowId (..))
import Moe.Infra.Setting.Effect (Setting, getSetting)
import Moe.Prelude
import System.FilePath (takeExtension, (</>))

-- | Result of an import scan operation.
data ImportResult = ImportResult
  { imported :: [ImportedEntry],
    skipped :: [SkippedEntry]
  }
  deriving stock (Eq, Show)

-- | A successfully imported bangumi entry.
data ImportedEntry = ImportedEntry
  { bangumiId :: Id Bangumi,
    title :: Text,
    posterUrl :: Maybe Text,
    maxEpisode :: Word32
  }
  deriving stock (Eq, Show)

-- | A skipped bangumi folder with the reason.
data SkippedEntry = SkippedEntry
  { folderName :: Text,
    reason :: SkipReason
  }
  deriving stock (Eq, Show)

-- | Reason for skipping a directory during import.
data SkipReason
  = AlreadyTracked
  | NoVideoFiles
  | TmdbSearchFailed
  deriving stock (Eq, Show)

-- | A single season scanned from a bangumi directory.
data ScannedBangumi = ScannedBangumi
  { folderName :: Text,
    parsedTitle :: Text,
    parsedYear :: Maybe Year,
    parsedTmdbId :: Maybe TmdbId,
    season :: SeasonIndex,
    maxEpisode :: Word32,
    videoFileCount :: Int
  }
  deriving stock (Eq, Show)

type BangumiCache = Map Text (Maybe Bangumi)

-- | Intermediate result after scanning and TMDB resolution.
data ResolvedDir
  = DirSkipped Text SkipReason
  | DirReady Text Bangumi Word32

-- | Scan the save path and import untracked bangumi.
--
-- Phase 1: Scan filesystem and resolve TMDB metadata (IO, no DB).
-- Phase 2: Persist all results in a single transaction.
importExistingBangumi ::
  (Setting :> es, Metadata :> es, Sqlite :> es, FileSystem :> es, Concurrent :> es, Log :> es, IOE :> es) =>
  Eff es ImportResult
importExistingBangumi = do
  pref <- getSetting
  let dlConfig = pref.downloader
      savePath = toString dlConfig.savePath
  exists <- doesDirectoryExist savePath
  if not exists
    then do
      Log.logAttention_ $ "import: save path does not exist: " <> toText savePath
      pure ImportResult {imported = [], skipped = []}
    else do
      dirs <- listDirectory savePath
      let bangumiDirs = filter (isBangumiDir . toText) dirs
      -- Phase 1: Scan filesystem and resolve TMDB metadata
      (_, resolved) <- foldM (resolveDirectory savePath) (Map.empty, []) bangumiDirs
      -- Phase 2: Persist all in a single transaction
      result <- transact $ foldM persistResolved (ImportResult [] []) (reverse resolved)
      pure result {imported = reverse result.imported, skipped = reverse result.skipped}

-- | Scan a directory and resolve its TMDB metadata (no DB operations).
resolveDirectory ::
  (Metadata :> es, FileSystem :> es, Log :> es, IOE :> es) =>
  FilePath ->
  (BangumiCache, [ResolvedDir]) ->
  FilePath ->
  Eff es (BangumiCache, [ResolvedDir])
resolveDirectory savePath (cache, results) dirName = do
  let dirPath = savePath </> dirName
      folderText = toText dirName
  scannedList <- scanBangumiDir dirPath folderText
  case scannedList of
    [] -> pure (cache, DirSkipped folderText NoVideoFiles : results)
    (sb0 : _) -> do
      (cache', mBangumi) <- resolveMetadata cache sb0.parsedTitle sb0.parsedTmdbId
      case mBangumi of
        Nothing ->
          pure (cache', DirSkipped folderText TmdbSearchFailed : results)
        Just bangumi ->
          let newResults =
                [ DirReady folderText (applySeason sb.season bangumi) sb.maxEpisode
                  | sb <- scannedList
                ]
           in pure (cache', reverse newResults <> results)

-- | Persist a resolved directory result within a transaction.
persistResolved ::
  (SqliteTransaction :> es, IOE :> es) =>
  ImportResult ->
  ResolvedDir ->
  Eff es ImportResult
persistResolved result (DirSkipped folder reason) =
  pure $ addSkipped result folder reason
persistResolved result (DirReady folderText bangumi maxEp) = do
  mExisting <- BangumiDB.findExistingBangumi bangumi
  case mExisting of
    Just entity -> do
      mTracking <- TrackingDB.getTrackingByBangumi entity.entityId
      case mTracking of
        Just _ -> pure $ addSkipped result folderText AlreadyTracked
        Nothing -> do
          entry <- createBangumiAndTracking bangumi maxEp
          pure $ addImported result entry
    Nothing -> do
      entry <- createBangumiAndTracking bangumi maxEp
      pure $ addImported result entry

-- | Create a bangumi and its tracking record within a transaction.
createBangumiAndTracking ::
  (SqliteTransaction :> es, IOE :> es) =>
  Bangumi ->
  Word32 ->
  Eff es ImportedEntry
createBangumiAndTracking bangumi maxEp = do
  (bid, _, _) <- BangumiDB.upsertBangumi bangumi
  void $
    TrackingDB.upsertTracking
      Tracking.Tracking
        { bangumiId = bid,
          trackingType = Tracking.Collection,
          rssUrl = Nothing,
          rssEnabled = False,
          lastPubdate = Nothing,
          currentEpisode = maxEp,
          episodeOffset = 0,
          isBDrip = False,
          autoComplete = True
        }
  pure
    ImportedEntry
      { bangumiId = bid,
        title = bangumi.titleChs,
        posterUrl = bangumi.posterUrl,
        maxEpisode = maxEp
      }

-- | Scan a bangumi directory for season subdirectories and video files.
--
-- Returns one 'ScannedBangumi' per season. Loose video files (not in any
-- Season directory) are grouped under Season 1.
scanBangumiDir ::
  (FileSystem :> es, IOE :> es) =>
  FilePath ->
  Text ->
  Eff es [ScannedBangumi]
scanBangumiDir dirPath folderText = do
  isDir <- doesDirectoryExist dirPath
  if not isDir
    then pure []
    else do
      let (title, year, tmdbId) = parseFolderName folderText
      entries <- listDirectory dirPath
      (seasonMap, looseFiles) <- foldM (scanEntry dirPath) (Map.empty, []) entries
      let -- Merge loose files into Season 1
          allSeasons = case looseFiles of
            [] -> seasonMap
            _ -> Map.insertWith (<>) (SeasonIndex 1) looseFiles seasonMap
      pure
        [ ScannedBangumi
            { folderName = folderText,
              parsedTitle = title,
              parsedYear = year,
              parsedTmdbId = tmdbId,
              season = sn,
              maxEpisode = extractMaxEpisode files,
              videoFileCount = length files
            }
          | (sn, files) <- Map.toAscList allSeasons,
            not (null files)
        ]

-- | Scan an entry within a bangumi directory (season folder or file).
scanEntry ::
  (FileSystem :> es, IOE :> es) =>
  FilePath ->
  (Map SeasonIndex [Text], [Text]) ->
  FilePath ->
  Eff es (Map SeasonIndex [Text], [Text])
scanEntry parentDir (seasonMap, looseFiles) entryName = do
  let entryPath = parentDir </> entryName
  isDir <- doesDirectoryExist entryPath
  if isDir
    then case parseSeasonDir (toText entryName) of
      Just sn -> do
        videoFiles <- listVideoFiles entryPath
        pure (Map.insertWith (<>) sn videoFiles seasonMap, looseFiles)
      Nothing -> pure (seasonMap, looseFiles)
    else
      let ext = T.drop 1 $ toText $ takeExtension entryName
       in if isVideoExt ext
            then pure (seasonMap, toText entryName : looseFiles)
            else pure (seasonMap, looseFiles)

-- | List all video files in a directory.
listVideoFiles :: (FileSystem :> es, IOE :> es) => FilePath -> Eff es [Text]
listVideoFiles dirPath = do
  entries <- listDirectory dirPath
  pure $ mapMaybe toVideoFile entries
  where
    toVideoFile f =
      let ext = T.drop 1 $ toText $ takeExtension f
       in if isVideoExt ext then Just (toText f) else Nothing

-- | Resolve metadata via TMDB using a cache to avoid duplicate lookups.
--
-- When a 'TmdbId' is available (parsed from folder name), fetches metadata
-- directly by ID (trying TV first, then Movie). Falls back to keyword search.
resolveMetadata ::
  (Metadata :> es, Log :> es) =>
  BangumiCache ->
  Text ->
  Maybe TmdbId ->
  Eff es (BangumiCache, Maybe Bangumi)
resolveMetadata cache title mTmdbId =
  case Map.lookup title cache of
    Just cached -> pure (cache, cached)
    Nothing -> do
      mBangumi <- case mTmdbId of
        Just (TmdbId tid) -> do
          let tid' = fromIntegral tid
          mTv <- getTmdbTvDetail (TvShowId tid')
          case mTv of
            Just _ -> pure mTv
            Nothing -> getTmdbMovieDetail (MovieId tid')
        Nothing -> viaNonEmpty head <$> searchTmdb title Nothing
      when (isNothing mBangumi) $
        Log.logAttention_ $ "import: TMDB search failed for " <> title
      let cache' = Map.insert title mBangumi cache
      pure (cache', mBangumi)

-- | Apply a season number to a Bangumi.
applySeason :: SeasonIndex -> Bangumi -> Bangumi
applySeason sn bangumi = case bangumi.kind of
  Movie -> bangumi
  _ -> bangumi {season = Just sn}

-- | Append an imported entry to the result.
addImported :: ImportResult -> ImportedEntry -> ImportResult
addImported r entry = r {imported = entry : r.imported}

-- | Prepend a skipped entry (reversed at the end).
addSkipped :: ImportResult -> Text -> SkipReason -> ImportResult
addSkipped r folder reason =
  r {skipped = SkippedEntry folder reason : r.skipped}
