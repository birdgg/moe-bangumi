module Moe.Infra.Metadata.Tmdb
  ( withTmdbClient,
    mkTmdbApi,
    tmdbResultToBangumi,
    tmdbTvDetailToBangumi,
    tmdbMovieDetailToBangumi,
    mediaTypeToKind,
  )
where

import Data.Text qualified as T
import Effectful ((:>))
import Moe.Domain.Bangumi (Bangumi (..), BangumiKind (..), SeasonNumber (..), TmdbId (..))
import Moe.Domain.Setting qualified as Setting
import Moe.Infra.Setting.Effect (Setting, getSetting)
import Moe.Prelude
import Network.HTTP.Client (Manager)
import Network.Tmdb qualified as Tmdb
import Network.Tmdb.Types.Image qualified as TmdbImage
import Network.Tmdb.Types.Movie (MovieDetail (..))
import Network.Tmdb.Types.Search (MediaType (..), MultiSearchResult (..))
import Network.Tmdb.Types.Tv (TvDetail (..))

-- | Run a TMDB action with a configured client, returning fallback if API key is empty.
withTmdbClient ::
  (Setting :> es) =>
  Manager ->
  a ->
  (Tmdb.TmdbApi -> Eff es a) ->
  Eff es a
withTmdbClient manager fallback action = do
  pref <- getSetting
  let cfg = Setting.tmdb pref
  if cfg.apiKey == ""
    then pure fallback
    else action (mkTmdbApi cfg manager)

-- | Create a TMDB API client.
mkTmdbApi :: Setting.TMDBConfig -> Manager -> Tmdb.TmdbApi
mkTmdbApi cfg = Tmdb.mkTmdbClient (Tmdb.TmdbConfig cfg.apiKey (parseLocale cfg.language))

-- | Convert a TMDB MultiSearchResult to Bangumi.
tmdbResultToBangumi :: MultiSearchResult -> Maybe Bangumi
tmdbResultToBangumi result = do
  date <- result.firstAirDate <|> result.releaseDate
  pure Bangumi
    { titleChs = fromMaybe "" (result.name <|> result.title),
      titleJap = result.originalName <|> result.originalTitle,
      airDate = date,
      season = Nothing,
      kind = mediaTypeToKind result.mediaType,
      mikanId = Nothing,
      tmdbId = Just (TmdbId (fromIntegral result.id)),
      bgmtvId = Nothing,
      posterUrl = TmdbImage.posterUrl TmdbImage.PosterW500 <$> result.posterPath,
      totalEpisodes = Nothing
    }

mediaTypeToKind :: MediaType -> BangumiKind
mediaTypeToKind MediaMovie = Movie
mediaTypeToKind MediaTv = Tv
mediaTypeToKind MediaPerson = Tv
mediaTypeToKind (MediaUnknown _) = Tv

-- | Convert a TMDB TvDetail to Bangumi.
tmdbTvDetailToBangumi :: TvDetail -> Maybe Bangumi
tmdbTvDetailToBangumi detail = do
  date <- detail.firstAirDate
  pure Bangumi
    { titleChs = detail.name,
      titleJap = Just detail.originalName,
      airDate = date,
      season = Just $ SeasonNumber $ fromIntegral detail.numberOfSeasons,
      kind = Tv,
      mikanId = Nothing,
      tmdbId = Just (TmdbId (fromIntegral detail.id.unTvShowId)),
      bgmtvId = Nothing,
      posterUrl = TmdbImage.posterUrl TmdbImage.PosterW500 <$> detail.posterPath,
      totalEpisodes = Just $ fromIntegral detail.numberOfEpisodes
    }

-- | Convert a TMDB MovieDetail to Bangumi.
tmdbMovieDetailToBangumi :: MovieDetail -> Maybe Bangumi
tmdbMovieDetailToBangumi detail = do
  date <- detail.releaseDate
  pure Bangumi
    { titleChs = detail.title,
      titleJap = Just detail.originalTitle,
      airDate = date,
      season = Nothing,
      kind = Movie,
      mikanId = Nothing,
      tmdbId = Just (TmdbId (fromIntegral detail.id.unMovieId)),
      bgmtvId = Nothing,
      posterUrl = TmdbImage.posterUrl TmdbImage.PosterW500 <$> detail.posterPath,
      totalEpisodes = Nothing
    }

-- | Parse locale text (e.g. "zh-CN") to TmdbLocale, defaulting to zh-CN.
parseLocale :: Text -> Tmdb.TmdbLocale
parseLocale t = case T.toLower t of
  "zh-cn" -> Tmdb.zhCN
  "zh-tw" -> Tmdb.zhTW
  "en-us" -> Tmdb.enUS
  "en-gb" -> Tmdb.enGB
  "ja-jp" -> Tmdb.jaJP
  "ko-kr" -> Tmdb.koKR
  "fr-fr" -> Tmdb.frFR
  "de-de" -> Tmdb.deDE
  "es-es" -> Tmdb.esES
  "es-mx" -> Tmdb.esMX
  "pt-br" -> Tmdb.ptBR
  "pt-pt" -> Tmdb.ptPT
  "it-it" -> Tmdb.itIT
  "ru-ru" -> Tmdb.ruRU
  _ -> Tmdb.zhCN

