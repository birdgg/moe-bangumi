{-# LANGUAGE FieldSelectors #-}

module Moe.Libs.Tmdb.API
  ( TmdbAPI,
    TmdbRoutes (..),
    MovieRoutes (..),
    SearchRoutes (..),
    TvRoutes (..),
    tmdbBaseUrl,
  )
where

import Moe.Libs.Tmdb.Types
import Moe.Prelude hiding ((:>))
import Servant.API (Capture, Get, JSON, NamedRoutes, QueryParam', Required, Strict, type (:-), type (:>))
import Servant.Client (BaseUrl (..), Scheme (..))

type TmdbAuth api =
  QueryParam' '[Required, Strict] "api_key" Text
    :> QueryParam' '[Required, Strict] "language" TmdbLocale
    :> api

type TmdbAPI = TmdbAuth (NamedRoutes TmdbRoutes)

data TmdbRoutes mode = TmdbRoutes
  { movie :: mode :- "movie" :> NamedRoutes MovieRoutes,
    search :: mode :- "search" :> NamedRoutes SearchRoutes,
    tv :: mode :- "tv" :> NamedRoutes TvRoutes
  }
  deriving stock (Generic)

data MovieRoutes mode = MovieRoutes
  { getMovieDetailRoute ::
      mode
        :- Capture "movie_id" MovieId
          :> Get '[JSON] MovieDetail
  }
  deriving stock (Generic)

data SearchRoutes mode = SearchRoutes
  { searchMultiRoute ::
      mode
        :- "multi"
          :> QueryParam' '[Required, Strict] "query" Text
          :> Get '[JSON] (PaginatedResponse MultiSearchResult)
  }
  deriving stock (Generic)

data TvRoutes mode = TvRoutes
  { getTvDetailRoute ::
      mode
        :- Capture "tv_id" TvShowId
          :> Get '[JSON] TvDetail
  }
  deriving stock (Generic)

tmdbBaseUrl :: BaseUrl
tmdbBaseUrl = BaseUrl Https "api.themoviedb.org" 443 "/3"
