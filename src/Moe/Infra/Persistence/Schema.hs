{-# OPTIONS_GHC -Wno-orphans #-}

module Moe.Infra.Persistence.Schema
  ( )
where

import Effectful.Sqlite (FromField (..), FromRow (..), ToField (..), ToRow (..), field)
import Moe.Core.Model.Bangumi (Bangumi (..), BangumiId (..))
import Moe.Core.Model.Internal.Types (BgmtvId (..), TmdbId (..))

instance FromField BangumiId where
  fromField = fmap BangumiId . fromField

instance ToField BangumiId where
  toField (BangumiId i) = toField i

instance FromField TmdbId where
  fromField = fmap TmdbId . fromField

instance ToField TmdbId where
  toField (TmdbId i) = toField i

instance FromField BgmtvId where
  fromField = fmap BgmtvId . fromField

instance ToField BgmtvId where
  toField (BgmtvId i) = toField i

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
