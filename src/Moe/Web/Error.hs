-- | Web-layer error types for API handlers.
module Moe.Web.Error
  ( WebError (..),
    throwNotFound,
    throwValidation,
    webErrorToServerError,
    jsonError,
  )
where

import Data.Aeson qualified as Aeson
import Data.Text.Display (Display (..))
import Moe.Prelude
import Servant (ServerError (..), err400, err404)

-- | Errors thrown by web API handlers.
data WebError
  = NotFound Text
  | ValidationError Text
  deriving stock (Show)

instance Display WebError where
  displayBuilder = \case
    NotFound msg -> "NotFound: " <> displayBuilder msg
    ValidationError msg -> "ValidationError: " <> displayBuilder msg

-- | Throw a NotFound error.
throwNotFound :: (Error WebError :> es) => Text -> Eff es a
throwNotFound = throwError . NotFound

-- | Throw a ValidationError.
throwValidation :: (Error WebError :> es) => Text -> Eff es a
throwValidation = throwError . ValidationError

-- | Convert WebError to Servant ServerError with JSON body.
webErrorToServerError :: WebError -> ServerError
webErrorToServerError = \case
  NotFound msg -> jsonError err404 msg
  ValidationError msg -> jsonError err400 msg

-- | Build a Servant ServerError with a JSON message body.
jsonError :: ServerError -> Text -> ServerError
jsonError base msg =
  base
    { errBody = Aeson.encode $ Aeson.object ["message" Aeson..= msg],
      errHeaders = [("Content-Type", "application/json")]
    }
