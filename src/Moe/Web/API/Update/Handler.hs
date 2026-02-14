-- | Handlers for update API endpoints.
module Moe.Web.API.Update.Handler
  ( handleGetAbout,
    handlePostUpdate,
  )
where

import Moe.Infra.Update.Effect (AboutInfo (..), checkForUpdate, detectPlatform, performUpdate)
import Moe.Prelude
import Moe.Web.API.DTO.Update (UpdateResponse (..), toAboutResponse)
import Moe.Web.API.DTO.Update qualified as DTO
import Moe.Web.Types (ServerEff)

-- | GET /api/about - version and update information.
handleGetAbout :: ServerEff DTO.AboutResponse
handleGetAbout = do
  about <- checkForUpdate
  platformInfo <- liftIO detectPlatform
  pure $ toAboutResponse about platformInfo

-- | POST /api/update - trigger self-update.
handlePostUpdate :: ServerEff UpdateResponse
handlePostUpdate = do
  about <- checkForUpdate
  if not about.needUpdate
    then pure UpdateResponse {success = False, message = "Already latest version"}
    else do
      performUpdate
      -- Process exits in performUpdate; this is unreachable
      pure UpdateResponse {success = True, message = "Updating..."}
