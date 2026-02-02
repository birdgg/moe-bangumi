module Moe.Web.API.Tracking.Handler
  ( handleListTracking,
    handleListTrackingWithBangumi,
    handleGetTracking,
    handleCreateTracking,
    handleUpdateTracking,
    handleDeleteTracking,
  )
where

import Data.Int (Int64)
import Data.Maybe (mapMaybe)
import Effectful.Error.Static (throwError)
import Effectful.Sqlite (notransact, transact)
import Moe.Infrastructure.Database.Tracking qualified as DB
import Moe.Domain.Tracking.Types (TrackingId (..))
import Moe.Error (MoeError (..))
import Moe.Web.API.DTO.Tracking
  ( CreateTrackingRequest,
    TrackingResponse,
    TrackingWithBangumiResponse,
    UpdateTrackingRequest,
    applyUpdateRequest,
    fromCreateRequest,
    toTrackingResponse,
    toTrackingWithBangumiResponse,
  )
import Moe.Web.Types (MoeEff)
import Servant (NoContent (..))

handleListTracking :: MoeEff [TrackingResponse]
handleListTracking = do
  trackings <- notransact DB.listTracking
  pure $ mapMaybe toTrackingResponse trackings

handleListTrackingWithBangumi :: MoeEff [TrackingWithBangumiResponse]
handleListTrackingWithBangumi = do
  results <- notransact DB.listTrackingWithBangumi
  pure $ mapMaybe (uncurry toTrackingWithBangumiResponse) results

handleGetTracking :: Int64 -> MoeEff TrackingResponse
handleGetTracking tid = do
  mTracking <- notransact $ DB.getTracking (TrackingId tid)
  case mTracking >>= toTrackingResponse of
    Just resp -> pure resp
    Nothing -> throwError $ NotFound "Tracking not found"

handleCreateTracking :: CreateTrackingRequest -> MoeEff TrackingResponse
handleCreateTracking req = do
  let tracking = fromCreateRequest req
  TrackingId newId <- transact $ DB.createTracking tracking
  handleGetTracking newId

handleUpdateTracking :: Int64 -> UpdateTrackingRequest -> MoeEff TrackingResponse
handleUpdateTracking tid req = do
  mTracking <- notransact $ DB.getTracking (TrackingId tid)
  case mTracking of
    Nothing -> throwError $ NotFound "Tracking not found"
    Just tracking -> do
      let updated = applyUpdateRequest req tracking
      transact $ DB.updateTracking updated
      handleGetTracking tid

handleDeleteTracking :: Int64 -> MoeEff NoContent
handleDeleteTracking tid = do
  transact $ DB.deleteTracking (TrackingId tid)
  pure NoContent
