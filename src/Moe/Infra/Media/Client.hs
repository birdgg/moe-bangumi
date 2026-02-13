-- | Emby HTTP client for media server operations.
module Moe.Infra.Media.Client
  ( getSystemInfo,
    getVirtualFolders,
    getUsers,
    getItems,
  )
where

import Control.Exception (try)
import Data.Aeson ((.:), (.:?))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.List (isInfixOf)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Moe.Infra.Media.Types (MediaClientError (..), MediaItem (..), MediaLibrary (..))
import Moe.Prelude
import Network.HTTP.Client
  ( HttpException,
    Manager,
    Request (..),
    httpLbs,
    parseRequest,
    responseBody,
    responseStatus,
  )
import Network.HTTP.Types.Status (statusCode, statusIsSuccessful)

-- | Build an Emby API request with normalized URL.
mkRequest :: Text -> Text -> Text -> IO (Maybe Request)
mkRequest baseUrl apiKey path = do
  let normalizedBase = T.dropWhileEnd (== '/') baseUrl
      url = toString normalizedBase <> toString path <> separator <> "api_key=" <> toString apiKey
      separator :: String
      separator = if "?" `isInfixOf` toString path then "&" else "?"
  req <- try @HttpException (parseRequest url)
  pure $ case req of
    Left _ -> Nothing
    Right r -> Just r {requestHeaders = [("Accept", "application/json")]}

-- | Get Emby system info (for testing connection).
getSystemInfo :: Manager -> Text -> Text -> IO (Either MediaClientError Text)
getSystemInfo manager baseUrl apiKey = do
  mReq <- mkRequest baseUrl apiKey "/emby/System/Info/Public"
  case mReq of
    Nothing -> pure $ Left (MediaInvalidUrl baseUrl)
    Just req -> do
      result <- try @HttpException $ httpLbs req manager
      pure $ case result of
        Left ex -> Left (MediaNetworkError (show ex))
        Right resp
          | statusIsSuccessful (responseStatus resp) ->
              case Aeson.decode (responseBody resp) >>= parseServerName of
                Just name -> Right name
                Nothing -> Right "Connected"
          | otherwise ->
              Left $ MediaHttpError (statusCode (responseStatus resp))

-- | Parse server name from system info JSON.
parseServerName :: Aeson.Value -> Maybe Text
parseServerName = Aeson.parseMaybe $ Aeson.withObject "SystemInfo" $ \obj ->
  obj .: "ServerName"

-- | Get all virtual folders (media libraries).
getVirtualFolders :: Manager -> Text -> Text -> IO (Either MediaClientError [MediaLibrary])
getVirtualFolders manager baseUrl apiKey = do
  mReq <- mkRequest baseUrl apiKey "/emby/Library/VirtualFolders"
  case mReq of
    Nothing -> pure $ Left (MediaInvalidUrl baseUrl)
    Just req -> do
      result <- try @HttpException $ httpLbs req manager
      pure $ case result of
        Left ex -> Left (MediaNetworkError (show ex))
        Right resp
          | statusIsSuccessful (responseStatus resp) ->
              case Aeson.eitherDecode (responseBody resp) of
                Right items -> Right (mapMaybe parseLibrary items)
                Left err -> Left (MediaParseError (toText err))
          | otherwise ->
              Left $ MediaHttpError (statusCode (responseStatus resp))

-- | Parse a library from Emby JSON.
parseLibrary :: Aeson.Value -> Maybe MediaLibrary
parseLibrary = Aeson.parseMaybe $ Aeson.withObject "Library" $ \obj -> do
  libraryId <- obj .: "ItemId"
  libraryName <- obj .: "Name"
  collectionType <- obj .:? "CollectionType"
  pure MediaLibrary {libraryId, libraryName, collectionType}

-- | Get the first Emby user ID.
getUsers :: Manager -> Text -> Text -> IO (Either MediaClientError Text)
getUsers manager baseUrl apiKey = do
  mReq <- mkRequest baseUrl apiKey "/emby/Users"
  case mReq of
    Nothing -> pure $ Left (MediaInvalidUrl baseUrl)
    Just req -> do
      result <- try @HttpException $ httpLbs req manager
      pure $ case result of
        Left ex -> Left (MediaNetworkError (show ex))
        Right resp
          | statusIsSuccessful (responseStatus resp) ->
              case Aeson.decode (responseBody resp) >>= parseFirstUserId of
                Just uid -> Right uid
                Nothing -> Left (MediaParseError "failed to parse users response")
          | otherwise ->
              Left $ MediaHttpError (statusCode (responseStatus resp))

-- | Parse the first user ID from Emby Users response.
parseFirstUserId :: Aeson.Value -> Maybe Text
parseFirstUserId = Aeson.parseMaybe $ Aeson.withArray "Users" $ \arr ->
  case toList arr of
    [] -> fail "no users found"
    (first_ : _) -> Aeson.withObject "User" (.: "Id") first_

-- | Get items from a specific library with user watch data.
getItems :: Manager -> Text -> Text -> Text -> Text -> IO (Either MediaClientError [MediaItem])
getItems manager baseUrl apiKey userId libraryId = do
  let path =
        "/emby/Users/"
          <> userId
          <> "/Items?ParentId="
          <> libraryId
          <> "&Recursive=true&IncludeItemTypes=Series,Movie&Fields=ProviderIds,PremiereDate,RecursiveItemCount"
  mReq <- mkRequest baseUrl apiKey path
  case mReq of
    Nothing -> pure $ Left (MediaInvalidUrl baseUrl)
    Just req -> do
      result <- try @HttpException $ httpLbs req manager
      pure $ case result of
        Left ex -> Left (MediaNetworkError (show ex))
        Right resp
          | statusIsSuccessful (responseStatus resp) ->
              case Aeson.decode (responseBody resp) >>= parseItemsResponse of
                Just items -> Right items
                Nothing -> Left (MediaParseError "failed to parse items response")
          | otherwise ->
              Left $ MediaHttpError (statusCode (responseStatus resp))

-- | Parse items from Emby Items response.
parseItemsResponse :: Aeson.Value -> Maybe [MediaItem]
parseItemsResponse = Aeson.parseMaybe $ Aeson.withObject "ItemsResponse" $ \obj -> do
  items <- obj .: "Items"
  mapM parseItem items

-- | Parse a single media item with user watch data.
parseItem :: Aeson.Value -> Aeson.Parser MediaItem
parseItem = Aeson.withObject "MediaItem" $ \obj -> do
  itemName <- obj .: "Name"
  itemType <- obj .: "Type"
  premiereDate <- obj .:? "PremiereDate"
  providerIdsVal <- obj .:? "ProviderIds"
  episodeCount <- obj .:? "RecursiveItemCount"
  userData <- obj .:? "UserData"
  let providerIds = fromMaybe Map.empty providerIdsVal
      playedCount = calcPlayedCount itemType episodeCount userData
  pure MediaItem {itemName, itemType, premiereDate, providerIds, episodeCount, playedCount}

-- | Calculate played episode count from Emby UserData.
-- For Series: RecursiveItemCount - UnplayedItemCount
-- For Movie: 1 if played, 0 otherwise
calcPlayedCount :: Text -> Maybe Word32 -> Maybe Aeson.Value -> Maybe Word32
calcPlayedCount itemType mEpisodeCount mUserData = case mUserData of
  Nothing -> Nothing
  Just ud -> Aeson.parseMaybe (parsePlayedCount itemType mEpisodeCount) ud

parsePlayedCount :: Text -> Maybe Word32 -> Aeson.Value -> Aeson.Parser Word32
parsePlayedCount itemType mEpisodeCount = Aeson.withObject "UserData" $ \obj ->
  case itemType of
    "Movie" -> do
      played <- obj .:? "Played"
      pure $ if fromMaybe False played then 1 else 0
    _ -> do
      unplayed <- obj .:? "UnplayedItemCount"
      case (mEpisodeCount, unplayed) of
        (Just total, Just unp) -> pure $ if total >= unp then total - unp else 0
        _ -> fail "missing episode count or unplayed count"
