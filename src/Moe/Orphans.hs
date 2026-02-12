{-# OPTIONS_GHC -Wno-orphans #-}

module Moe.Orphans () where

import Data.Time.Calendar (Year)
import Moe.Prelude
import Data.Text.Display (Display (..))
import Network.HTTP.Types.Status (statusCode)
import Servant.Client (ClientError (..))
import Servant.Client.Core (ResponseF (..))

instance ToText Year where
  toText = show

instance Display ClientError where
  displayBuilder = \case
    FailureResponse _ Response {responseStatusCode} ->
      "HTTP error: " <> displayBuilder (show @Text (statusCode responseStatusCode))
    ConnectionError e ->
      "Connection error: " <> displayBuilder (show @Text e)
    other ->
      "Client error: " <> displayBuilder (show @Text other)