module Moe.Web.API.Tracking.Handler
  ( handleListTracking,
    handleListTrackingWithBangumi,
    handleGetTracking,
    handleCreateTracking,
    handleUpdateTracking,
    handleDeleteTracking,
    handleRefreshTracking,
  )
where

import Moe.Domain.Shared.Entity (Entity (..), Id (..))
import Moe.Domain.Shared.Metadata (BgmtvId (..))
import Moe.Domain.Bangumi (Bangumi (..))
import Moe.Domain.Tracking (Tracking (..), TrackingType (..))
import Moe.Web.Error (throwNotFound)
import Moe.Infra.Database.Bangumi qualified as BangumiDB
import Moe.Infra.Database.Tracking qualified as DB
import Moe.Infra.Metadata.Effect (getBangumiEpisodeOffset)
import Moe.Infra.Database.Episode qualified as EpisodeDB
import Moe.Job.Subscription.Process (triggerSingleSubscription)
import Moe.Prelude
import Moe.Web.API.DTO.Tracking
  ( CreateTrackingRequest (..),
    TrackingResponse,
    TrackingWithBangumiResponse,
    UpdateTrackingRequest (..),
    applyUpdateRequest,
    fromCreateRequest,
    toTrackingResponse,
    toTrackingWithBangumiResponse,
  )
import Moe.Web.Types (ServerEff)
import Servant (NoContent (..))
import Web.Bgmtv.Types.Id (SubjectId (..))

handleListTracking :: ServerEff [TrackingResponse]
handleListTracking = do
  trackings <- notransact DB.listTracking
  pure $ map toTrackingResponse trackings

handleListTrackingWithBangumi :: ServerEff [TrackingWithBangumiResponse]
handleListTrackingWithBangumi = do
  results <- notransact DB.listTrackingWithBangumi
  pure $ map (uncurry toTrackingWithBangumiResponse) results

handleGetTracking :: Int64 -> ServerEff TrackingResponse
handleGetTracking tid = do
  mTracking <- notransact $ DB.getTracking (Id tid)
  case mTracking of
    Just entity -> pure $ toTrackingResponse entity
    Nothing -> throwNotFound "Tracking not found"

handleCreateTracking :: CreateTrackingRequest -> ServerEff TrackingResponse
handleCreateTracking req = do
  mBangumi <- notransact $ BangumiDB.getBangumi (Id req.bangumiId)
  autoOffset <- case req.episodeOffset of
    Just _ -> pure 0
    Nothing -> detectEpisodeOffset mBangumi
  let tracking = fromCreateRequest autoOffset req
  Id newId <- transact $ DB.createTracking tracking
  when (tracking.trackingType == Subscription) $
    whenJust tracking.rssUrl $ \url ->
      whenJust mBangumi $ \bangumi ->
        triggerSingleSubscription bangumi url tracking.episodeOffset
  handleGetTracking newId

-- | Auto-detect episode offset from Bangumi.tv metadata.
detectEpisodeOffset :: Maybe (Entity Bangumi) -> ServerEff Word32
detectEpisodeOffset mBangumi = do
  let mBgmtvId = mBangumi >>= \e -> e.entityVal.bgmtvId
  case mBgmtvId of
    Just (BgmtvId bid) -> do
      offset <- getBangumiEpisodeOffset (SubjectId (fromIntegral bid))
      pure $ round (max 0 offset)
    Nothing -> pure 0

handleUpdateTracking :: Int64 -> UpdateTrackingRequest -> ServerEff TrackingResponse
handleUpdateTracking tid req = do
  mTracking <- notransact $ DB.getTracking (Id tid)
  case mTracking of
    Nothing -> throwNotFound "Tracking not found"
    Just entity -> do
      let updated = applyUpdateRequest req entity
      transact $ DB.updateTracking updated
      when (isJust req.rssUrl && updated.entityVal.trackingType == Subscription) $
        whenJust updated.entityVal.rssUrl $ \url -> do
          mBangumi <- notransact $ BangumiDB.getBangumi updated.entityVal.bangumiId
          whenJust mBangumi $ \bangumi ->
            triggerSingleSubscription bangumi url updated.entityVal.episodeOffset
      handleGetTracking tid

handleDeleteTracking :: Int64 -> ServerEff NoContent
handleDeleteTracking tid = do
  transact $ DB.deleteTracking (Id tid)
  pure NoContent

handleRefreshTracking :: Int64 -> ServerEff NoContent
handleRefreshTracking tid = do
  mTracking <- notransact $ DB.getTracking (Id tid)
  case mTracking of
    Nothing -> throwNotFound "Tracking not found"
    Just entity -> do
      transact $ EpisodeDB.deleteEpisodesByBangumi entity.entityVal.bangumiId
      let cleared = entity {entityVal = entity.entityVal {lastPubdate = Nothing}}
      transact $ DB.updateTracking cleared
      whenJust entity.entityVal.rssUrl $ \url -> do
        mBangumi <- notransact $ BangumiDB.getBangumi entity.entityVal.bangumiId
        whenJust mBangumi $ \bangumi ->
          triggerSingleSubscription bangumi url entity.entityVal.episodeOffset
      pure NoContent
