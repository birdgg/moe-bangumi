module Moe.Infra.Rss.Types
  ( RawItem (..),
    RssFetchError (..),
  )
where

import Data.Aeson (ToJSON)
import Data.OpenApi (ToSchema)
import Data.Text.Display (Display (..))
import Moe.Domain.Rss (PubDate)
import Moe.Prelude

data RawItem = RawItem
  { title :: Maybe Text,
    pubDate :: Maybe PubDate,
    torrentUrl :: Maybe Text,
    infoHash :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, ToSchema)

-- | Structured RSS fetch errors.
data RssFetchError
  = -- | Network-level failure (timeout, DNS, connection refused)
    RssNetworkError Text
  | -- | Server returned non-2xx status code
    RssHttpError Int
  | -- | XML or content parsing failure
    RssParseError Text
  deriving stock (Show, Eq)

instance Display RssFetchError where
  displayBuilder = \case
    RssNetworkError msg -> "Rss network error: " <> displayBuilder msg
    RssHttpError code -> "Rss HTTP " <> displayBuilder (show @Text code)
    RssParseError msg -> "Rss parse error: " <> displayBuilder msg
