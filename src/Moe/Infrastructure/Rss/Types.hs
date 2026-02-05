module Moe.Infrastructure.Rss.Types
  ( RawItem (..),
    RssError (..),
  )
where

import Data.Aeson (ToJSON)
import Data.OpenApi (ToSchema)
import Data.Text.Display (Display (..))
import Moe.Domain.Rss.Types (PubDate)
import Moe.Prelude

data RawItem = RawItem
  { title :: Maybe Text,
    pubDate :: Maybe PubDate,
    torrentUrl :: Maybe Text,
    infoHash :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, ToSchema)

data RssError
  = NetworkError Text
  | XmlParseError Text
  deriving stock (Show, Eq)
  deriving anyclass (Exception)

instance Display RssError where
  displayBuilder (NetworkError msg) = "Network error: " <> displayBuilder msg
  displayBuilder (XmlParseError msg) = "XML parse error: " <> displayBuilder msg
