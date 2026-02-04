module Moe.Infrastructure.Download.Types
  ( Tag (..),
    TagList (..),
    DownloadError (..),
    fromTagText,
  )
where

import Data.Text qualified as T
import Data.Text.Display (Display (..))
import Moe.Prelude

data DownloadError
  = LoginFailed Text
  | AddTorrentFailed Text
  | ConfigMissing
  | InvalidConfig Text
  deriving stock (Eq, Show)

instance Display DownloadError where
  displayBuilder (LoginFailed msg) = "Login failed: " <> displayBuilder msg
  displayBuilder (AddTorrentFailed msg) = "Add torrent failed: " <> displayBuilder msg
  displayBuilder ConfigMissing = "Downloader config missing"
  displayBuilder (InvalidConfig field) = "Invalid config: " <> displayBuilder field <> " is empty"

data Tag = Moe | Rename | Subscription | Collection
  deriving stock (Eq, Show, Bounded, Enum)

instance ToText Tag where
  toText Moe = "moe"
  toText Rename = "rename"
  toText Subscription = "subscription"
  toText Collection = "collection"

fromTagText :: Text -> Maybe Tag
fromTagText = inverseMap toText

newtype TagList = TagList [Tag]
  deriving stock (Eq, Show)

instance ToText TagList where
  toText (TagList ts) = T.intercalate "," $ map toText ts
