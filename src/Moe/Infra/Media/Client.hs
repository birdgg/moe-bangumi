-- | Plex Media Server HTTP client.
module Moe.Infra.Media.Client
  ( refreshPlexLibrary,
  )
where

import Control.Exception (try)
import Moe.Prelude hiding (try)
import Network.HTTP.Client (HttpException, Manager, httpLbs, parseRequest, responseStatus)
import Network.HTTP.Types.Status (statusIsSuccessful)

-- | Trigger a Plex library refresh via GET request.
refreshPlexLibrary :: (IOE :> es) => Manager -> Text -> Text -> Eff es (Either Text ())
refreshPlexLibrary manager plexUrl plexToken = do
  let url = toString plexUrl <> "/library/sections/all/refresh?X-Plex-Token=" <> toString plexToken
  result <- liftIO $ try @HttpException $ do
    req <- parseRequest url
    httpLbs req manager
  pure $ case result of
    Left err ->
      Left ("Plex network error: " <> show err)
    Right response
      | statusIsSuccessful (responseStatus response) -> Right ()
      | otherwise ->
          Left ("Plex HTTP error: " <> show (responseStatus response))
