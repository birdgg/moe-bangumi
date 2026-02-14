module Moe.Error
  ( AppError (..),
  )
where

import Data.Text.Display (Display (..))
import Moe.Infra.Database.Types (DatabaseExecError)
import Moe.Infra.Downloader.Types (DownloaderClientError)
import Moe.Infra.Metadata.Types (MetadataFetchError)
import Moe.Infra.Rss.Types (RssFetchError)
import Moe.Infra.Update.Types (UpdateClientError)
import Moe.Prelude

-- | Application-level error types.
data AppError
  = RssError RssFetchError
  | DownloaderError DownloaderClientError
  | MetadataError MetadataFetchError
  | DatabaseError DatabaseExecError
  | UpdateError UpdateClientError
  | NotFound Text
  | ValidationError Text
  deriving stock (Show)
  deriving anyclass (Exception)

instance Display AppError where
  displayBuilder = \case
    RssError err -> displayBuilder err
    DownloaderError err -> displayBuilder err
    MetadataError err -> displayBuilder err
    DatabaseError err -> displayBuilder err
    UpdateError err -> displayBuilder err
    NotFound msg -> "NotFound: " <> displayBuilder msg
    ValidationError msg -> "ValidationError: " <> displayBuilder msg
