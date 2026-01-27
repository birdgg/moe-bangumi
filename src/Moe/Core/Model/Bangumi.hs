module Moe.Core.Model.Bangumi
  ( BangumiId (..),
    TmdbId (..),
    BgmtvId (..),
    Bangumi (..),
  )
where

import Moe.Core.Model.Internal.Types (BgmtvId (..), TmdbId (..))

newtype BangumiId = BangumiId Int64
  deriving stock (Eq, Show)

data Bangumi = Bangumi
  { id :: Maybe BangumiId,
    name :: Text,
    year :: Maybe Word16,
    tmdbId :: Maybe TmdbId,
    bangumiTvId :: Maybe BgmtvId,
    posterUrl :: Maybe Text,
    overview :: Maybe Text
  }
  deriving stock (Eq, Show)
