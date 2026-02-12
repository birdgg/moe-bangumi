module Moe.Infra.Metadata.Types
  ( Keyword,
    MetadataFetchError (..),
    classifyClientError,
    classifyProviderError,
  )
where

import Data.Text.Display (Display (..))
import Moe.Prelude
import Servant.Client (ClientError (..))
import Servant.Client.Core (ResponseF (..))
import Network.HTTP.Types.Status (statusCode)

-- | Search keyword.
type Keyword = Text

-- | Structured metadata fetch errors.
data MetadataFetchError
  = MetaNetworkError Text
  | MetaHttpError Int
  | MetaParseError Text
  | MetaConfigError Text
  deriving stock (Show, Eq)

instance Display MetadataFetchError where
  displayBuilder = \case
    MetaNetworkError msg -> "Metadata: network error: " <> displayBuilder msg
    MetaHttpError code -> "Metadata: HTTP " <> displayBuilder (show @Text code)
    MetaParseError msg -> "Metadata: parse error: " <> displayBuilder msg
    MetaConfigError msg -> "Metadata: config error: " <> displayBuilder msg

-- | Classify a Servant ClientError into MetadataFetchError.
classifyClientError :: ClientError -> MetadataFetchError
classifyClientError = \case
  FailureResponse _ Response {responseStatusCode} -> MetaHttpError (statusCode responseStatusCode)
  ConnectionError ex -> MetaNetworkError (show ex)
  DecodeFailure msg _ -> MetaParseError (toText msg)
  other -> MetaNetworkError (show other)

-- | Classify any Show-able provider error into MetadataFetchError.
classifyProviderError :: (Show e) => e -> MetadataFetchError
classifyProviderError = MetaNetworkError . show
