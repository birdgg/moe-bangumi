{-# OPTIONS_GHC -Wno-orphans #-}

module Moe.Infrastructure.Database.Orphans () where

import Data.Text (Text)
import Data.Text qualified as T
import Effectful.Sqlite (FromField (..), FromRow (..), ToField (..), ToRow (..), field)
import Moe.Domain.Bangumi.Types
  ( Bangumi (..),
    BangumiId (..),
    BangumiKind (..),
    BgmtvId (..),
    MikanId (..),
    TmdbId (..),
  )
import Moe.Domain.Bangumi.Episode.Types
  ( Episode (..),
    EpisodeId (..),
    EpisodeNumber (..),
  )
import Moe.Domain.Tracking.Types
  ( Tracking (..),
    TrackingId (..),
    TrackingType (..),
  )
import Relude (ToText (..))

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

instance FromField MikanId where
  fromField = fmap MikanId . fromField

instance ToField MikanId where
  toField (MikanId i) = toField i

instance FromField BangumiKind where
  fromField f = do
    t <- fromField @Text f
    case t of
      "tv" -> pure Tv
      "web" -> pure Web
      "movie" -> pure Movie
      "ova" -> pure Ova
      _ -> fail $ "Invalid kind: " <> T.unpack t

instance ToField BangumiKind where
  toField = toField . toText

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
      <*> field
      <*> field
      <*> field

instance ToRow Bangumi where
  toRow b =
    toRow
      ( b.id,
        b.titleChs,
        b.titleJap,
        b.airDate,
        b.season,
        b.kind,
        b.mikanId,
        b.tmdbId,
        b.bgmtvId,
        b.posterUrl
      )

instance FromField TrackingId where
  fromField = fmap TrackingId . fromField

instance ToField TrackingId where
  toField (TrackingId i) = toField i

instance FromField TrackingType where
  fromField f = do
    t <- fromField @Text f
    case t of
      "subscription" -> pure Subscription
      "collection" -> pure Collection
      _ -> fail $ "Invalid tracking type: " <> T.unpack t

instance ToField TrackingType where
  toField = toField . toText

instance FromRow Tracking where
  fromRow =
    Tracking
      <$> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field

instance ToRow Tracking where
  toRow t =
    toRow
      ( t.id,
        t.bangumiId,
        t.trackingType,
        t.rssUrl,
        t.rssEnabled,
        t.lastPubdate,
        t.currentEpisode,
        t.createdAt
      )

instance FromField EpisodeId where
  fromField = fmap EpisodeId . fromField

instance ToField EpisodeId where
  toField (EpisodeId i) = toField i

instance FromField EpisodeNumber where
  fromField = fmap EpisodeNumber . fromField

instance ToField EpisodeNumber where
  toField (EpisodeNumber n) = toField n

instance FromRow Episode where
  fromRow =
    Episode
      <$> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field

instance ToRow Episode where
  toRow e =
    toRow
      ( e.id,
        e.bangumiId,
        e.episodeNumber,
        e.subtitleGroup,
        e.resolution,
        e.infoHash,
        e.torrentUrl,
        e.pubDate,
        e.createdAt
      )
