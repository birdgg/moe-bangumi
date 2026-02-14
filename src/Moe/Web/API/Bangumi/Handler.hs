module Moe.Web.API.Bangumi.Handler
  ( handleSearchTmdb,
    handleUpdateBangumiTmdbId,
    handleGetEpisodeOffset,
  )
where

import Data.Time.Calendar (Year)
import Effectful.Sqlite (notransact, transact)
import Moe.Domain.Bangumi qualified as Types
import Moe.Domain.Shared.Entity (Entity (..), Id (..))
import Moe.Domain.Shared.Metadata (BgmtvId (..), TmdbId (..))
import Moe.Error (AppError (..))
import Moe.Infra.Database.Bangumi qualified as DB
import Moe.Infra.Metadata.Effect (getBangumiEpisodeOffset, searchTmdb)
import Moe.Prelude
import Moe.Web.API.DTO.Bangumi
  ( TmdbSearchResult,
    UpdateBangumiTmdbIdRequest (..),
    toTmdbSearchResult,
  )
import Moe.Web.Types (ServerEff)
import Web.Bgmtv.Types.Id (SubjectId (..))

-- | Search TMDB by keyword. The year parameter is accepted but not used for
-- filtering, since filterByAirDate does exact Day matching which is too strict
-- for a user-facing search (e.g. 2026-01-01 won't match 2026-04-05).
handleSearchTmdb :: Text -> Maybe Year -> ServerEff [TmdbSearchResult]
handleSearchTmdb keyword _maybeYear = do
  results <- searchTmdb keyword Nothing
  pure $ mapMaybe toTmdbSearchResult results

-- | Update a bangumi's TMDB ID.
handleUpdateBangumiTmdbId :: Int64 -> UpdateBangumiTmdbIdRequest -> ServerEff ()
handleUpdateBangumiTmdbId bid req = do
  mEntity <- notransact $ DB.getBangumi (Id bid)
  case mEntity of
    Nothing -> throwError $ NotFound "Bangumi not found"
    Just entity -> do
      let b = entity.entityVal
          newTmdbId = TmdbId <$> req.tmdbId
          updated = b {Types.tmdbId = newTmdbId} :: Types.Bangumi
      transact $ DB.updateBangumi (entity {entityVal = updated})

-- | Get episode offset from Bangumi.tv metadata.
handleGetEpisodeOffset :: Int64 -> ServerEff Word32
handleGetEpisodeOffset bid = do
  mEntity <- notransact $ DB.getBangumi (Id bid)
  case mEntity of
    Nothing -> throwError $ NotFound "Bangumi not found"
    Just entity -> case entity.entityVal.bgmtvId of
      Just (BgmtvId bgmId) -> do
        offset <- getBangumiEpisodeOffset (SubjectId (fromIntegral bgmId))
        pure $ round (max 0 offset)
      Nothing -> pure 0
