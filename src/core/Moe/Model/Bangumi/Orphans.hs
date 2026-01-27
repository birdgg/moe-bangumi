{-# OPTIONS_GHC -Wno-orphans #-}

module Moe.Model.Bangumi.Orphans () where

import Effectful.Sqlite (FromField (..), FromRow (..), SQLData (..), ToField (..), ToRow (..), field)
import Moe.Model.Bangumi.Types

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

instance FromField AnimeSeason where
  fromField f = do
    txt <- fromField @Text f
    case parseAnimeSeasonText txt of
      Just s -> pure s
      Nothing -> fail "Invalid anime season format"

instance ToField AnimeSeason where
  toField s = SQLText (animeSeasonToStorageText s)

parseAnimeSeasonText :: Text -> Maybe AnimeSeason
parseAnimeSeasonText txt = do
  let (yearPart, rest) = splitAt 4 (toString txt)
  year <- readMaybe yearPart
  s <- case rest of
    "Winter" -> Just Winter
    "Spring" -> Just Spring
    "Summer" -> Just Summer
    "Fall" -> Just Fall
    _ -> Nothing
  pure $ AnimeSeason year s

animeSeasonToStorageText :: AnimeSeason -> Text
animeSeasonToStorageText (AnimeSeason y s) = show y <> seasonText s
  where
    seasonText Winter = "Winter"
    seasonText Spring = "Spring"
    seasonText Summer = "Summer"
    seasonText Fall = "Fall"

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

instance ToRow Bangumi where
  toRow b =
    toRow
      ( b.id,
        b.name,
        b.year,
        b.animeSeason,
        b.mikanId,
        b.tmdbId,
        b.bgmtvId,
        b.posterUrl,
        b.overview
      )
