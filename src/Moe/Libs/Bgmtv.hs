module Moe.Libs.Bgmtv
  ( BgmtvConfig (..),
    defaultConfig,
    BgmtvClient (..),
    newBgmtvClientWith,
    module Moe.Libs.Bgmtv.Types,
  )
where

import Moe.Libs.Bgmtv.API qualified as API
import Moe.Libs.Bgmtv.Types
import Moe.Prelude
import Network.HTTP.Client (Manager)
import Servant.Client (BaseUrl, ClientEnv, ClientM, mkClientEnv, runClientM)
import Servant.Client.Generic (AsClientT, genericClient)

data BgmtvConfig = BgmtvConfig
  { userAgent :: Text,
    baseUrl :: BaseUrl
  }
  deriving stock (Show, Eq)

defaultConfig :: Text -> BgmtvConfig
defaultConfig userAgent =
  BgmtvConfig
    { userAgent,
      baseUrl = API.bgmtvBaseUrl
    }

data BgmtvClient = BgmtvClient
  { searchSubjects :: SearchRequest -> IO (Response SearchResponse),
    getSubject :: SubjectId -> IO (Response SubjectDetail),
    getEpisodes :: SubjectId -> Maybe Int64 -> Maybe Int64 -> IO (Response EpisodesResponse)
  }

newBgmtvClientWith :: Manager -> BgmtvConfig -> BgmtvClient
newBgmtvClientWith manager config =
  BgmtvClient
    { searchSubjects = \req ->
        run (API.searchSubjects client' config.userAgent req),
      getSubject = \subjectId ->
        run (API.getSubject client' subjectId config.userAgent),
      getEpisodes = \subjectId limit offset ->
        run (API.getEpisodes client' config.userAgent subjectId limit offset)
    }
  where
    run :: ClientM a -> IO (Response a)
    run action = first fromClientError <$> runClientM action env

    env :: ClientEnv
    env = mkClientEnv manager config.baseUrl

client' :: API.BgmtvRoutes (AsClientT ClientM)
client' = genericClient
