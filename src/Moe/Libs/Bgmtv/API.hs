{-# LANGUAGE FieldSelectors #-}

module Moe.Libs.Bgmtv.API
  ( BgmtvRoutes (..),
    bgmtvBaseUrl,
  )
where

import Moe.Libs.Bgmtv.Types
import Moe.Prelude hiding ((:>))
import Servant.API
  ( Capture,
    Get,
    Header',
    JSON,
    Post,
    QueryParam,
    QueryParam',
    ReqBody,
    Required,
    Strict,
    type (:-),
    type (:>),
  )
import Servant.Client (BaseUrl (..), Scheme (..))

data BgmtvRoutes mode = BgmtvRoutes
  { searchSubjects ::
      mode
        :- "v0"
          :> "search"
          :> "subjects"
          :> Header' '[Required, Strict] "User-Agent" Text
          :> ReqBody '[JSON] SearchRequest
          :> Post '[JSON] SearchResponse,
    getSubject ::
      mode
        :- "v0"
          :> "subjects"
          :> Capture "id" SubjectId
          :> Header' '[Required, Strict] "User-Agent" Text
          :> Get '[JSON] SubjectDetail,
    getEpisodes ::
      mode
        :- "v0"
          :> "episodes"
          :> Header' '[Required, Strict] "User-Agent" Text
          :> QueryParam' '[Required, Strict] "subject_id" SubjectId
          :> QueryParam "limit" Int64
          :> QueryParam "offset" Int64
          :> Get '[JSON] EpisodesResponse
  }
  deriving stock (Generic)

bgmtvBaseUrl :: BaseUrl
bgmtvBaseUrl = BaseUrl Https "api.bgm.tv" 443 ""
