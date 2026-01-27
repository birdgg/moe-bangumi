module Moe.Model.Bangumi
  ( BangumiId (..),
    TmdbId (..),
    BangumiTvId (..),
    Bangumi (..),
  )
where

import Effectful.Sqlite (FromField (..), FromRow (..), ToField (..), ToRow (..), field)

newtype BangumiId = BangumiId Int64
  deriving stock (Eq, Show)
  deriving newtype (FromField, ToField)

newtype TmdbId = TmdbId Word32
  deriving stock (Eq, Show)
  deriving newtype (Num, FromField, ToField)

newtype BangumiTvId = BangumiTvId Word32
  deriving stock (Eq, Show)
  deriving newtype (Num, FromField, ToField)

data Bangumi = Bangumi
  { id :: Maybe BangumiId,
    name :: Text,
    year :: Maybe Word16,
    tmdbId :: Maybe TmdbId,
    bangumiTvId :: Maybe BangumiTvId,
    posterUrl :: Maybe Text,
    overview :: Maybe Text
  }
  deriving stock (Eq, Show)

instance FromRow Bangumi where
  fromRow =
    Bangumi
      <$> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field

instance ToRow Bangumi where
  toRow b =
    toRow
      ( b.id,
        b.name,
        b.year,
        b.tmdbId,
        b.bangumiTvId,
        b.posterUrl,
        b.overview
      )
