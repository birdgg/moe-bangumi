module Moe.Libs.Tmdb
  ( TmdbApi (..),
    TmdbConfig (..),
    mkTmdbClient,
    module Moe.Libs.Tmdb.Types,
  )
where

import Moe.Libs.Tmdb.API qualified as API
import Moe.Libs.Tmdb.Types
import Moe.Prelude
import Network.HTTP.Client (Manager)
import Servant.Client (ClientEnv, ClientM, client, mkClientEnv, runClientM)
import Servant.Client.Generic (AsClientT)

data TmdbConfig = TmdbConfig
  { apiKey :: Text,
    language :: TmdbLocale
  }
  deriving stock (Show, Eq)

data TmdbApi = TmdbApi
  { searchMulti :: Text -> IO (Either TmdbClientError (PaginatedResponse MultiSearchResult)),
    getMovieDetail :: MovieId -> IO (Either TmdbClientError MovieDetail),
    getTvDetail :: TvShowId -> IO (Either TmdbClientError TvDetail)
  }

mkTmdbClient :: TmdbConfig -> Manager -> TmdbApi
mkTmdbClient cfg manager =
  TmdbApi
    { searchMulti = \keyword -> run (API.searchMultiRoute (API.search routes) keyword),
      getMovieDetail = \movieId -> run (API.getMovieDetailRoute (API.movie routes) movieId),
      getTvDetail = \tvId -> run (API.getTvDetailRoute (API.tv routes) tvId)
    }
  where
    env :: ClientEnv
    env = mkClientEnv manager API.tmdbBaseUrl

    routes :: API.TmdbRoutes (AsClientT ClientM)
    routes = client (Proxy @API.TmdbAPI) cfg.apiKey cfg.language

    run :: ClientM a -> IO (Either TmdbClientError a)
    run action = first fromClientError <$> runClientM action env
