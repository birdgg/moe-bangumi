-- | Handlers for media server endpoints.
module Moe.Web.API.Media.Handler
  ( handleTestMedia,
    handleListLibraries,
    handleImportLibrary,
  )
where

import Control.Exception.Safe (tryAny)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time.Calendar (Day)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import Effectful.Sqlite (transact)
import Moe.Domain.Bangumi (Bangumi (..))
import Moe.Domain.Shared.Entity (Entity (..))
import Moe.Domain.Tracking (Tracking (..), TrackingType (..))
import Moe.Infra.Database.Bangumi qualified as DB
import Moe.Infra.Database.Tracking qualified as DB
import Effectful.Reader.Static qualified as Reader
import Moe.App.Env (MoeEnv (..))
import Moe.Error (AppError (..))
import Moe.Infra.Media.Client qualified as Client
import Moe.Infra.Media.Effect (testMediaConnection)
import Moe.Infra.Media.Types (MediaItem (..), MediaLibrary)
import Moe.Infra.Metadata.Effect (getTmdbMovieDetail, getTmdbTvDetail, searchBgmtv, searchTmdb)
import Moe.Prelude
import Moe.Web.API.DTO.Media
import Moe.Web.Types (ServerEff)
import Network.Tmdb (MovieId (..), TvShowId (..))

-- | Test an Emby connection with the given credentials.
handleTestMedia :: TestMediaRequest -> ServerEff TestMediaResponse
handleTestMedia req = do
  result <- testMediaConnection req.url req.apiKey
  pure $ case result of
    Left errMsg ->
      TestMediaResponse
        { success = False,
          serverName = Nothing,
          message = errMsg
        }
    Right name ->
      TestMediaResponse
        { success = True,
          serverName = Just name,
          message = "Connected"
        }

-- | List available media libraries using explicit credentials.
handleListLibraries :: TestMediaRequest -> ServerEff [MediaLibrary]
handleListLibraries req = do
  env <- Reader.ask @MoeEnv
  result <- liftIO $ Client.getVirtualFolders env.httpManager req.url req.apiKey
  case result of
    Left err -> throwError $ MediaError err
    Right libs -> pure libs

-- | Import all items from a media library using explicit credentials.
handleImportLibrary :: ImportLibraryRequest -> ServerEff ImportResult
handleImportLibrary req = do
  env <- Reader.ask @MoeEnv
  result <- liftIO $ Client.getItems env.httpManager req.url req.apiKey req.libraryId
  items <- case result of
    Left err -> throwError $ MediaError err
    Right xs -> pure xs
  results <- mapM importItem items
  let imported = length $ filter id results
      failed = length $ filter not results
  pure ImportResult {imported, failed}

-- | Import a single media item. Returns True on success, False on failure.
importItem :: MediaItem -> ServerEff Bool
importItem item = do
  result <- tryAny $ do
    mBangumi <- resolveBangumi item
    case mBangumi of
      Nothing -> pure False
      Just bangumi -> do
        transact $ do
          bangumiId <- case bangumi.tmdbId of
            Just tid -> do
              existing <- DB.findBangumiByTmdbId tid
              case existing of
                Just entity -> do
                  DB.updateBangumi entity {entityVal = mergeBangumi entity.entityVal bangumi}
                  pure entity.entityId
                Nothing -> do
                  (bid, _, _) <- DB.upsertBangumi bangumi
                  pure bid
            Nothing -> do
              (bid, _, _) <- DB.upsertBangumi bangumi
              pure bid
          void $
            DB.upsertTracking
              Tracking
                { bangumiId = bangumiId,
                  trackingType = Collection,
                  rssUrl = Nothing,
                  rssEnabled = False,
                  lastPubdate = Nothing,
                  currentEpisode = fromMaybe 0 item.episodeCount,
                  episodeOffset = 0,
                  isBDrip = False,
                  autoComplete = True
                }
        pure True
  case result of
    Left _err -> pure False
    Right success -> pure success

-- | Resolve bangumi metadata from a media item.
-- Priority: TMDB ID from provider IDs -> TMDB search -> Bgmtv search.
resolveBangumi :: MediaItem -> ServerEff (Maybe Bangumi)
resolveBangumi item = do
  let mTmdbId = extractTmdbId item
  case mTmdbId of
    Just tmdbId -> case item.itemType of
      "Movie" -> getTmdbMovieDetail (MovieId (fromIntegral tmdbId))
      _ -> getTmdbTvDetail (TvShowId (fromIntegral tmdbId))
    Nothing -> searchByName item

-- | Search for bangumi by name via TMDB then Bgmtv.
searchByName :: MediaItem -> ServerEff (Maybe Bangumi)
searchByName item = do
  let airDate = parsePremiereDate =<< item.premiereDate
  tmdbResults <- searchTmdb item.itemName airDate
  case tmdbResults of
    (b : _) -> pure (Just b)
    [] -> do
      bgmtvResults <- searchBgmtv item.itemName airDate
      pure $ listToMaybe bgmtvResults

-- | Extract TMDB ID from provider IDs map.
extractTmdbId :: MediaItem -> Maybe Word32
extractTmdbId item = do
  tmdbStr <- Map.lookup "Tmdb" item.providerIds
  readMaybe $ toString tmdbStr

-- | Parse premiere date from Emby format to Day.
parsePremiereDate :: Text -> Maybe Day
parsePremiereDate t =
  let dateStr = toString $ T.take 10 t
   in parseTimeM True defaultTimeLocale "%Y-%m-%d" dateStr

-- | Merge new bangumi data into existing, preferring existing non-NULL values.
mergeBangumi :: Bangumi -> Bangumi -> Bangumi
mergeBangumi existing new_ =
  Bangumi
    { titleChs = existing.titleChs,
      titleJap = existing.titleJap <|> new_.titleJap,
      airDate = existing.airDate,
      season = existing.season <|> new_.season,
      kind = existing.kind,
      mikanId = existing.mikanId <|> new_.mikanId,
      tmdbId = existing.tmdbId <|> new_.tmdbId,
      bgmtvId = existing.bgmtvId <|> new_.bgmtvId,
      posterUrl = existing.posterUrl <|> new_.posterUrl,
      totalEpisodes = existing.totalEpisodes <|> new_.totalEpisodes
    }
