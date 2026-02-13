-- | Emby HTTP client for media server operations.
module Moe.Infra.Media.Client
  ( getSystemInfo,
    getVirtualFolders,
    getUsers,
    getItems,
  )
where

import Data.Text qualified as T
import Moe.Infra.Media.Emby.API (EmbyAPI, EmbyRoutes (..))
import Moe.Infra.Media.Emby.Types qualified as Emby
import Moe.Infra.Media.Types
import Moe.Prelude
import Network.HTTP.Client (Manager)
import Servant.Client

-- | Emby client functions generated from NamedRoutes.
EmbyRoutes
  { systemInfo = getSystemInfoC,
    virtualFolders = getVirtualFoldersC,
    users = getUsersC,
    items = getItemsC
  } = client (Proxy @EmbyAPI)

-- | Parse BaseUrl from user-provided Text.
parseEmbyBaseUrl :: Text -> Maybe BaseUrl
parseEmbyBaseUrl url =
  let normalized = toString $ T.dropWhileEnd (== '/') url
   in rightToMaybe $ parseBaseUrl normalized

-- | Run a Servant client action against an Emby server.
runEmbyClient :: Manager -> Text -> ClientM a -> IO (Either MediaClientError a)
runEmbyClient manager baseUrl action =
  case parseEmbyBaseUrl baseUrl of
    Nothing -> pure $ Left (MediaInvalidUrl baseUrl)
    Just base -> do
      let env = mkClientEnv manager base
      result <- runClientM action env
      pure $ first classifyMediaClientError result

-- | Get Emby system info (for testing connection).
getSystemInfo :: Manager -> Text -> Text -> IO (Either MediaClientError Text)
getSystemInfo manager baseUrl apiKey =
  fmap (.serverName) <$> runEmbyClient manager baseUrl (getSystemInfoC apiKey)

-- | Get all virtual folders (media libraries).
getVirtualFolders :: Manager -> Text -> Text -> IO (Either MediaClientError [MediaLibrary])
getVirtualFolders manager baseUrl apiKey =
  fmap (map Emby.toMediaLibrary) <$> runEmbyClient manager baseUrl (getVirtualFoldersC apiKey)

-- | Get the first Emby user ID.
getUsers :: Manager -> Text -> Text -> IO (Either MediaClientError Text)
getUsers manager baseUrl apiKey = do
  result <- runEmbyClient manager baseUrl (getUsersC apiKey)
  pure $ result >>= \case
    [] -> Left (MediaParseError "no users found")
    (first_ : _) -> Right first_.userId

-- | Get items from a specific library with user watch data.
getItems :: Manager -> Text -> Text -> Text -> Text -> IO (Either MediaClientError [MediaItem])
getItems manager baseUrl apiKey userId libraryId =
  fmap (map Emby.toMediaItem . (.items))
    <$> runEmbyClient
      manager
      baseUrl
      ( getItemsC
          userId
          libraryId
          True
          ("Series,Movie" :: Text)
          ("ProviderIds,PremiereDate,RecursiveItemCount" :: Text)
          apiKey
      )
