-- | Collection rename strategy.
--
-- Traverses torrent files once, resolves metadata via BangumiMap cache,
-- renames all files to media server naming conventions, then persists
-- resolved bangumi and tracking records to the database.
module Moe.Job.Torrent.Rename.Collection
  ( renameCollection,

    -- * Exported for testing
    processFile,
  )
where

import Control.Monad (foldM)
import Data.Char (isDigit)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Effectful.Log qualified as Log
import Moe.Infra.Database.Types (DatabaseExecError)
import Moe.Domain.File
  ( ContentKind (..),
    Subtitle,
    BangumiFile (..),
    BangumiMeta (..),
    EpisodeIndex (..),
    GroupName (..),
    generateFullPath,
    isSubtitleExt,
    isVideoExt,
    mkContent,
    mkContentSub,
    toBangumiMeta,
  )
import Moe.Domain.File qualified as File
import Moe.Domain.Parser.Collection (ParsedDirName (..), ParsedInfo (..), ParsedSpContent (..), extractGroup, parseCollectionDirName, parseInfo, parseSpContent)
import Moe.Domain.Bangumi (Bangumi (..), SeasonIndex (..))
import Moe.Domain.Bangumi qualified as Bangumi
import Moe.Domain.Tracking qualified as Tracking
import Moe.Infra.Database.Bangumi qualified as BangumiDB
import Moe.Infra.Database.Tracking qualified as TrackingDB
import Moe.Infra.Downloader.Effect
import Moe.Infra.Metadata.Effect (Metadata, searchTmdb)
import Moe.Infra.Database.PendingNotification (PendingNotification (..))
import Moe.Infra.Database.PendingNotification qualified as PendingNotificationDB
import Moe.Prelude

type BangumiFolder = Text

-- | Cached resolution results for each bangumi directory.
type BangumiMap = Map BangumiFolder (Maybe Bangumi)

-- | Parsed media information from a file.
data ParsedMediaInfo
  = ParsedSpecial EpisodeIndex
  | ParsedMovie
  | ParsedEpisode (Maybe SeasonIndex) EpisodeIndex
  | ParsedExtra ContentKind

-- | Rename a collection torrent by traversing files and resolving metadata on demand.
renameCollection ::
  (Downloader :> es, Metadata :> es, Sqlite :> es, Error DatabaseExecError :> es, Concurrent :> es, Log :> es, IOE :> es) =>
  TorrentInfo ->
  Text ->
  Eff es ()
renameCollection torrent hash = do
  let bdrip = T.isInfixOf "bdrip" (T.toLower torrent.name)
      grp = maybeToList $ GroupName <$> extractGroup torrent.name
  files <- getTorrentFiles hash
  let wantedFiles = filter (\f -> f.priority /= 0) files
  bmap <- foldM (\bmap file -> do
    (bmap', mNewPath) <- processFile bdrip grp bmap file.name
    forM_ mNewPath $ \newPath -> do
      result <- try @SomeException $ renameTorrentFile hash file.name newPath
      case result of
        Left ex -> Log.logAttention_ $ "rename: failed " <> file.name <> " -> " <> newPath <> " - " <> toText (displayException ex)
        Right () -> pass
    pure bmap'
    ) Map.empty wantedFiles
  when (Map.null bmap) $
    Log.logAttention_ $ "collection: no bangumi dirs found in " <> torrent.name

  persistBangumiMap bdrip bmap
  removeTagsFromTorrents [hash] [renameTag]
  startTorrents [hash]
  transact $ forM_ (Map.elems bmap) $ \case
    Just nb -> do
      let suffix = if bdrip then " BDRip" else ""
      PendingNotificationDB.insertPendingNotification
        PendingNotification
          { infoHash = hash,
            title = nb.titleChs <> suffix,
            posterUrl = nb.posterUrl
          }
    Nothing -> pass
  Log.logInfo_ $ "Renamed collection: " <> torrent.name

-- | Process a single file: find its bangumi directory, resolve metadata, and compute new path.
--
-- Files inside special directories (SPs, CDs, Fonts, etc.) are moved with
-- their original sub-directory structure preserved under the season folder.
processFile ::
  (Metadata :> es, Log :> es) =>
  Bool ->
  [GroupName] ->
  BangumiMap ->
  Text ->
  Eff es (BangumiMap, Maybe Text)
processFile bdrip grp bmap fileName = case findBangumiDir fileName of
  Nothing -> pure (bmap, Nothing)
  Just dirName -> do
    let parsed = parseCollectionDirName dirName
    (bmap', resolved) <- case Map.lookup dirName bmap of
      Just resolved -> pure (bmap, resolved)
      Nothing -> do
        results <- searchTmdb parsed.keyword Nothing
        let resolved = withSeason parsed.season <$> viaNonEmpty head results
        when (null results) $
          Log.logAttention_ $ parsed.keyword <> " search tmdb failed, using parsed title"
        pure (Map.insert dirName resolved bmap, resolved)
    let meta = case resolved of
          Just nb -> (toBangumiMeta nb) {group = grp, isBDrip = bdrip}
          Nothing -> fallbackMeta parsed grp bdrip
        isMovie = maybe False (\nb -> nb.kind == Bangumi.Movie) resolved
        newPath = computeNewPath meta isMovie dirName fileName
    pure (bmap', Just newPath)

-- | Apply parsed season to a Bangumi. Movies have no season.
withSeason :: Maybe SeasonIndex -> Bangumi -> Bangumi
withSeason parsedSeason nb =
  let s = case nb.kind of
        Bangumi.Movie -> Nothing
        _ -> parsedSeason <|> Just (SeasonIndex 1)
   in nb {Bangumi.season = s}

-- | Persist resolved bangumi and create Collection tracking records.
persistBangumiMap ::
  (Sqlite :> es, Error DatabaseExecError :> es, Concurrent :> es, Log :> es, IOE :> es) =>
  Bool ->
  BangumiMap ->
  Eff es ()
persistBangumiMap bdrip bmap =
  transact $ forM_ (Map.elems bmap) $ \case
    Just nb -> do
      (bid, _, _) <- BangumiDB.upsertBangumi nb
      void $
        TrackingDB.upsertTracking
          Tracking.Tracking
            { bangumiId = bid,
              trackingType = Tracking.Collection,
              rssUrl = Nothing,
              rssEnabled = False,
              lastPubdate = Nothing,
              currentEpisode = 0,
              episodeOffset = 0,
              isBDrip = bdrip,
              autoComplete = True
            }
    Nothing -> pass

-- | Build a fallback BangumiMeta from parsed directory name when TMDB search fails.
fallbackMeta :: ParsedDirName -> [GroupName] -> Bool -> BangumiMeta
fallbackMeta parsed grp bdrip =
  BangumiMeta
    { title = parsed.keyword,
      tmdbId = Nothing,
      season = parsed.season <|> Just (SeasonIndex 1),
      group = grp,
      isBDrip = bdrip
    }

-- | Parse media information from a filename.
parseMediaInfo :: Text -> Bool -> BangumiMeta -> ParsedMediaInfo
parseMediaInfo fileName isMovie baseMeta =
  let baseName = T.takeWhileEnd (/= '/') fileName
   in case parseSpContent baseName of
        Just (ExtraContent content) -> ParsedExtra content
        Just (SpecialEpisode ep) -> ParsedSpecial ep
        Nothing
          | isMovie -> ParsedMovie
          | otherwise ->
              let parsed = parseInfo baseName
                  seasonNum = baseMeta.season <|> parsed.season <|> Just (SeasonIndex 1)
                  ep = fromMaybe 1 parsed.episodeNumber
               in ParsedEpisode seasonNum ep

-- | Compute the new path for a file within a collection.
computeNewPath :: BangumiMeta -> Bool -> Text -> Text -> Text
computeNewPath meta isMovie dirName fileName =
  let relPath = fileRelativePath dirName fileName
      lastExt = T.toLower $ T.takeWhileEnd (/= '.') fileName
   in if isVideoExt lastExt
        then buildVideoPath meta isMovie fileName
        else case extractSubtitleInfo fileName of
          Just (sub, subExt) -> buildSubtitlePath meta isMovie fileName sub subExt
          Nothing -> buildNonVideoPath meta relPath

buildVideoPath :: BangumiMeta -> Bool -> Text -> Text
buildVideoPath baseMeta isMovie fileName =
  let baseName = T.takeWhileEnd (/= '/') fileName
      ext = T.toLower $ T.takeWhileEnd (/= '.') baseName
      mkFile meta content = BangumiFile {bangumi = meta, content, ext}
      mkPath meta kind = toText $ generateFullPath (mkFile meta (mkContent kind))
   in case parseMediaInfo fileName isMovie baseMeta of
        ParsedExtra kind -> mkPath baseMeta kind
        ParsedSpecial ep -> mkPath baseMeta (Special ep)
        ParsedMovie -> mkPath baseMeta File.Movie
        ParsedEpisode seasonNum ep ->
          let meta = baseMeta {File.season = seasonNum} :: BangumiMeta
           in mkPath meta (Episode ep)

-- | Extract subtitle language and extension from a filename (e.g. "xxx.CHS.ass" -> Just (CHS, "ass")).
extractSubtitleInfo :: Text -> Maybe (Subtitle, Text)
extractSubtitleInfo fileName =
  let baseName = T.takeWhileEnd (/= '/') fileName
      ext = T.toLower $ T.takeWhileEnd (/= '.') baseName
   in if isSubtitleExt ext
        then
          let withoutExt = T.dropEnd (T.length ext + 1) baseName
              langTag = T.takeWhileEnd (/= '.') withoutExt
           in (,ext) <$> fromText langTag
        else Nothing

-- | Build new path for a subtitle file, mirroring video file naming with language tag.
buildSubtitlePath :: BangumiMeta -> Bool -> Text -> Subtitle -> Text -> Text
buildSubtitlePath baseMeta isMovie fileName sub subExt =
  let mkFile meta content = BangumiFile {bangumi = meta, content, ext = subExt}
      mkPath meta kind = toText $ generateFullPath (mkFile meta (mkContentSub kind sub))
   in case parseMediaInfo fileName isMovie baseMeta of
        ParsedSpecial ep -> mkPath baseMeta (Special ep)
        ParsedMovie -> mkPath baseMeta File.Movie
        ParsedEpisode seasonNum ep ->
          let meta = baseMeta {File.season = seasonNum} :: BangumiMeta
           in mkPath meta (Episode ep)
        ParsedExtra _ ->
          buildNonVideoPath baseMeta (fileRelativePath (fromMaybe "" (findBangumiDir fileName)) fileName)

-- | Build new path for a non-video file, preserving relative structure under season directory.
buildNonVideoPath :: BangumiMeta -> Text -> Text
buildNonVideoPath meta relPath =
  let base = toText (File.showBaseName meta)
   in case meta.season of
        Nothing -> base <> "/" <> relPath
        Just s -> base <> "/" <> toText (File.seasonDir s) <> "/" <> relPath

-- | Find the bangumi directory for a file.
--
-- For 4+ segments (root/dir/.../file), returns the second segment unless it is
-- a season directory or a special media directory, in which case the first
-- segment is used (torrent root IS the bangumi dir).
-- For 3 segments, returns the first segment.
findBangumiDir :: Text -> Maybe Text
findBangumiDir fileName =
  let segments = T.splitOn "/" fileName
   in case segments of
        (_ : dir : _ : _)
          | not (isSeasonDir (T.toLower dir))
          , not (T.toLower dir `Set.member` specialDirNames) ->
              Just dir
        (dir : _ : _) -> Just dir
        _ -> Nothing

-- | Check if a directory name is a season directory (e.g. "season 01", "season01").
isSeasonDir :: Text -> Bool
isSeasonDir dir = case T.stripPrefix "season" dir of
  Just rest -> T.all (\c -> isDigit c || c == ' ') rest
  Nothing -> False

-- | Known special directory names that don't represent bangumi.
specialDirNames :: Set Text
specialDirNames =
  Set.fromList
    [ "bonus", "cd", "cds", "extra", "extras", "font", "fonts",
      "menu", "menus", "ost", "pv", "pvs", "scan", "scans",
      "soundtrack", "soundtracks", "sp", "special", "specials",
      "sps", "trailer", "trailers"
    ]

-- | Extract relative path from bangumi directory for a file.
fileRelativePath :: Text -> Text -> Text
fileRelativePath bangumiDir fileName =
  case dropWhile (/= bangumiDir) (T.splitOn "/" fileName) of
    (_ : rest) -> T.intercalate "/" rest
    _ -> fileName
