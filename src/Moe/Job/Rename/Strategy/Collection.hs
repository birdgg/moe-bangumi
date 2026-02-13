-- | Collection rename strategy.
--
-- Traverses torrent files once, resolves metadata via BangumiMap cache,
-- renames all files to media server naming conventions, then persists
-- resolved bangumi and tracking records to the database.
module Moe.Job.Rename.Strategy.Collection
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
import Effectful
import Effectful.Exception (try)
import Effectful.Log qualified as Log
import Effectful.Sqlite (transact)
import Moe.Domain.File
  ( BangumiContent (Episode, EpisodeSub, MovieSub),
    BangumiFile (..),
    BangumiMeta (..),
    GroupName (..),
    Year,
    generateFullPath,
    isSubtitleExt,
    isVideoExt,
    toBangumiMeta,
  )
import Moe.Domain.Shared.Subtitle (Subtitle)
import Moe.Domain.File qualified as File
import Moe.Domain.Parser.Collection (ParsedDirName (..), ParsedInfo (..), ParsedSpContent (..), extractGroup, parseCollectionDirName, parseInfo, parseSpContent)
import Moe.Domain.Bangumi (Bangumi (..), BangumiKind (..), SeasonNumber (..))
import Moe.Domain.Bangumi qualified as Bangumi
import Moe.Domain.Tracking qualified as Tracking
import Moe.Infra.Database.Bangumi qualified as BangumiDB
import Moe.Infra.Database.Tracking qualified as TrackingDB
import Moe.Infra.Downloader.Effect
import Moe.Infra.Metadata.Effect (Metadata, searchTmdb)
import Moe.Infra.Notification.Effect (Notification)
import Moe.Job.Rename.Util (notifySafe)
import Moe.Prelude

type BangumiFolder = Text

-- | Cached resolution results for each bangumi directory.
type BangumiMap = Map BangumiFolder (Maybe Bangumi)

-- | Rename a collection torrent by traversing files and resolving metadata on demand.
renameCollection ::
  (Downloader :> es, Metadata :> es, Notification :> es, Sqlite :> es, Concurrent :> es, Log :> es, IOE :> es) =>
  TorrentInfo ->
  Text ->
  Eff es ()
renameCollection torrent hash = do
  let bdrip = T.isInfixOf "bdrip" (T.toLower torrent.name)
      grp = maybeToList $ GroupName <$> extractGroup torrent.name
  files <- getTorrentFiles hash
  bmap <- foldM (\bmap file -> do
    (bmap', mNewPath) <- processFile bdrip grp bmap file.name
    forM_ mNewPath $ \newPath -> do
      result <- try @SomeException $ renameTorrentFile hash file.name newPath
      case result of
        Left ex -> Log.logAttention_ $ "rename: failed " <> file.name <> " -> " <> newPath <> " - " <> toText (displayException ex)
        Right () -> pass
    pure bmap'
    ) Map.empty files
  when (Map.null bmap) $
    Log.logAttention_ $ "collection: no bangumi dirs found in " <> torrent.name

  persistBangumiMap bdrip bmap
  removeTagsFromTorrents [hash] [renameTag]
  startTorrents [hash]
  forM_ (Map.elems bmap) $ \case
    Just nb -> do
      let suffix = if bdrip then " BDRip" else ""
      notifySafe (nb.titleChs <> suffix) nb.posterUrl
    Nothing -> pass
  Log.logInfo_ $ "Renamed collection: " <> torrent.name

-- | Process a single file: find its bangumi directory, resolve metadata, and compute new path.
processFile ::
  (Metadata :> es, Log :> es) =>
  Bool ->
  [GroupName] ->
  BangumiMap ->
  Text ->
  Eff es (BangumiMap, Maybe Text)
processFile bdrip grp bmap fileName
  | containsSpecialDir fileName = pure (bmap, Nothing)
  | otherwise = case findBangumiDir fileName of
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
          movieYear = resolved >>= \nb ->
            if nb.kind == Movie then Just (Bangumi.extractYear nb.airDate) else Nothing
          newPath = computeNewPath meta movieYear dirName fileName
      pure (bmap', Just newPath)

-- | Apply parsed season to a Bangumi. Movies have no season.
withSeason :: Maybe SeasonNumber -> Bangumi -> Bangumi
withSeason parsedSeason nb =
  let s = case nb.kind of
        Movie -> Nothing
        _ -> parsedSeason <|> Just (SeasonNumber 1)
   in nb {Bangumi.season = s}

-- | Persist resolved bangumi and create Collection tracking records.
persistBangumiMap ::
  (Sqlite :> es, Concurrent :> es, Log :> es, IOE :> es) =>
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
      year = Nothing,
      tmdbId = Nothing,
      season = parsed.season <|> Just (SeasonNumber 1),
      group = grp,
      isBDrip = bdrip
    }

-- | Compute the new path for a file within a collection.
computeNewPath :: BangumiMeta -> Maybe Year -> Text -> Text -> Text
computeNewPath meta movieYear dirName fileName =
  let relPath = fileRelativePath dirName fileName
      lastExt = T.toLower $ T.takeWhileEnd (/= '.') fileName
   in if isVideoExt lastExt
        then buildVideoPath meta movieYear fileName
        else case extractSubtitleInfo fileName of
          Just (sub, subExt) -> buildSubtitlePath meta movieYear fileName sub subExt
          Nothing -> buildNonVideoPath meta relPath

buildVideoPath :: BangumiMeta -> Maybe Year -> Text -> Text
buildVideoPath baseMeta movieYear fileName =
  let baseName = T.takeWhileEnd (/= '/') fileName
      ext = T.toLower $ T.takeWhileEnd (/= '.') baseName
      mkFile meta content = BangumiFile {bangumi = meta, content, ext}
      mkPath meta content = toText $ generateFullPath (mkFile meta content)
   in case parseSpContent baseName of
        Just (ExtraContent content) -> mkPath baseMeta content
        Just (SpecialEpisode ep) ->
          let meta = baseMeta {File.season = Just (SeasonNumber 0)} :: BangumiMeta
           in mkPath meta (Episode ep)
        Nothing
          | Just year <- movieYear ->
              mkPath baseMeta (File.Movie year)
          | otherwise ->
              let parsed = parseInfo baseName
                  seasonNum = baseMeta.season <|> parsed.season <|> Just (SeasonNumber 1)
                  meta = baseMeta {File.season = seasonNum} :: BangumiMeta
               in mkPath meta (Episode (fromMaybe 1 parsed.episodeNumber))

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
buildSubtitlePath :: BangumiMeta -> Maybe Year -> Text -> Subtitle -> Text -> Text
buildSubtitlePath baseMeta movieYear fileName sub subExt =
  let baseName = T.takeWhileEnd (/= '/') fileName
      mkFile meta content = BangumiFile {bangumi = meta, content, ext = subExt}
      mkPath meta content = toText $ generateFullPath (mkFile meta content)
   in case parseSpContent baseName of
        Just (SpecialEpisode ep) ->
          let meta = baseMeta {File.season = Just (SeasonNumber 0)} :: BangumiMeta
           in mkPath meta (EpisodeSub ep sub)
        Nothing
          | Just year <- movieYear ->
              mkPath baseMeta (MovieSub year sub)
          | otherwise ->
              let parsed = parseInfo baseName
                  seasonNum = baseMeta.season <|> parsed.season <|> Just (SeasonNumber 1)
                  meta = baseMeta {File.season = seasonNum} :: BangumiMeta
               in mkPath meta (EpisodeSub (fromMaybe 1 parsed.episodeNumber) sub)
        _ -> buildNonVideoPath baseMeta (fileRelativePath (fromMaybe "" (findBangumiDir fileName)) fileName)

-- | Build new path for a non-video file, preserving relative structure under season directory.
buildNonVideoPath :: BangumiMeta -> Text -> Text
buildNonVideoPath meta relPath =
  let base = toText (File.showBaseName meta)
   in case meta.season of
        Nothing -> base <> "/" <> relPath
        Just s -> base <> "/" <> toText (File.seasonDir s) <> "/" <> relPath

-- | Find the bangumi directory for a file.
--
-- For 3+ segments (root/dir/file), returns the second segment (skipping season dirs).
-- For 2+ segments, returns the first segment (torrent root IS the bangumi dir).
findBangumiDir :: Text -> Maybe Text
findBangumiDir fileName =
  let segments = T.splitOn "/" fileName
   in case segments of
        (_ : dir : _ : _)
          | not (isSeasonDir (T.toLower dir)) -> Just dir
        (dir : _ : _) -> Just dir
        _ -> Nothing

-- | Check if any path segment is a known special media directory.
containsSpecialDir :: Text -> Bool
containsSpecialDir fileName =
  any (\s -> T.toLower s `Set.member` specialDirNames) (T.splitOn "/" fileName)

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
  let segments = T.splitOn "/" fileName
   in case segments of
        (_ : dir : rest)
          | dir == bangumiDir -> T.intercalate "/" rest
        (dir : rest)
          | dir == bangumiDir -> T.intercalate "/" rest
        _ -> fileName

