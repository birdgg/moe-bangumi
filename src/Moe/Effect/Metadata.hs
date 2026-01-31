module Moe.Effect.Metadata
  ( Metadata (..),
    searchBgmtv,
    searchTmdb,
    searchBangumiData,
    getBgmtvDetail,
    getTmdbTvDetail,
    getTmdbMovieDetail,
    fetchBangumiDataBySeason,
  )
where

import Data.Text (Text)
import Data.Word (Word16, Word32)
import Effectful
import Effectful.TH (makeEffect)
import Moe.Domain.Bangumi.Types (BangumiSeason)
import Moe.Infra.BangumiData.Types (BangumiDataItem)
import Network.Tmdb.Types.Movie (MovieDetail)
import Network.Tmdb.Types.Search (MultiSearchResult)
import Network.Tmdb.Types.Tv (TvDetail)
import Web.Bgmtv.Types (Subject, SubjectDetail)

data Metadata :: Effect where
  SearchBgmtv :: Text -> Maybe Word16 -> Metadata m [Subject]
  SearchTmdb :: Text -> Maybe Word16 -> Metadata m [MultiSearchResult]
  SearchBangumiData :: Text -> Maybe Word16 -> Metadata m [BangumiDataItem]
  GetBgmtvDetail :: Word32 -> Metadata m (Maybe SubjectDetail)
  GetTmdbTvDetail :: Word32 -> Metadata m (Maybe TvDetail)
  GetTmdbMovieDetail :: Word32 -> Metadata m (Maybe MovieDetail)
  FetchBangumiDataBySeason :: BangumiSeason -> Metadata m [BangumiDataItem]

makeEffect ''Metadata
