-- | Emby API response types with FromJSON instances.
module Moe.Infra.Media.Emby.Types
  ( -- * Response types
    SystemInfo (..),
    EmbyUser (..),
    EmbyLibrary (..),
    EmbyItem (..),
    ItemsResponse (..),
    UserData (..),

    -- * Conversions
    toMediaLibrary,
    toMediaItem,
  )
where

import Data.Aeson (FromJSON, (.:), (.:?))
import Data.Aeson qualified as Aeson
import Data.Map.Strict qualified as Map
import Moe.Infra.Media.Types (MediaItem (..), MediaLibrary (..))
import Moe.Prelude

-- | Emby system info response.
newtype SystemInfo = SystemInfo
  { serverName :: Text
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON SystemInfo where
  parseJSON = Aeson.withObject "SystemInfo" $ \obj ->
    SystemInfo <$> obj .: "ServerName"

-- | Emby user.
newtype EmbyUser = EmbyUser
  { userId :: Text
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON EmbyUser where
  parseJSON = Aeson.withObject "EmbyUser" $ \obj ->
    EmbyUser <$> obj .: "Id"

-- | Emby virtual folder (media library).
data EmbyLibrary = EmbyLibrary
  { itemId :: Text,
    name :: Text,
    collectionType :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON EmbyLibrary where
  parseJSON = Aeson.withObject "EmbyLibrary" $ \obj ->
    EmbyLibrary
      <$> obj .: "ItemId"
      <*> obj .: "Name"
      <*> obj .:? "CollectionType"

-- | Convert EmbyLibrary to MediaLibrary.
toMediaLibrary :: EmbyLibrary -> MediaLibrary
toMediaLibrary lib =
  MediaLibrary
    { libraryId = lib.itemId,
      libraryName = lib.name,
      collectionType = lib.collectionType
    }

-- | Emby items response wrapper.
newtype ItemsResponse = ItemsResponse
  { items :: [EmbyItem]
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON ItemsResponse where
  parseJSON = Aeson.withObject "ItemsResponse" $ \obj ->
    ItemsResponse <$> obj .: "Items"

-- | Emby media item.
data EmbyItem = EmbyItem
  { name :: Text,
    itemType :: Text,
    premiereDate :: Maybe Text,
    providerIds :: Maybe (Map Text Text),
    recursiveItemCount :: Maybe Word32,
    userData :: Maybe UserData
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON EmbyItem where
  parseJSON = Aeson.withObject "EmbyItem" $ \obj ->
    EmbyItem
      <$> obj .: "Name"
      <*> obj .: "Type"
      <*> obj .:? "PremiereDate"
      <*> obj .:? "ProviderIds"
      <*> obj .:? "RecursiveItemCount"
      <*> obj .:? "UserData"

-- | Emby user watch data.
data UserData = UserData
  { played :: Maybe Bool,
    unplayedItemCount :: Maybe Word32
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON UserData where
  parseJSON = Aeson.withObject "UserData" $ \obj ->
    UserData
      <$> obj .:? "Played"
      <*> obj .:? "UnplayedItemCount"

-- | Convert EmbyItem to MediaItem.
toMediaItem :: EmbyItem -> MediaItem
toMediaItem item =
  MediaItem
    { itemName = item.name,
      itemType = item.itemType,
      premiereDate = item.premiereDate,
      providerIds = fromMaybe Map.empty item.providerIds,
      episodeCount = item.recursiveItemCount,
      playedCount = calcPlayedCount item.itemType item.recursiveItemCount item.userData
    }

-- | Calculate played episode count from Emby UserData.
calcPlayedCount :: Text -> Maybe Word32 -> Maybe UserData -> Maybe Word32
calcPlayedCount itemType mEpisodeCount = \case
  Nothing -> Nothing
  Just ud -> case itemType of
    "Movie" -> Just $ if fromMaybe False ud.played then 1 else 0
    _ -> do
      total <- mEpisodeCount
      unplayed <- ud.unplayedItemCount
      pure $ if total >= unplayed then total - unplayed else 0
