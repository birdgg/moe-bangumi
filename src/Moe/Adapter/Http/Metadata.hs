module Moe.Adapter.Http.Metadata
  ( runMetadataHttp,
  )
where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word16)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static (Error, throwError)
import Moe.App.Error (MoeError (..))
import Moe.Domain.Bangumi.Types qualified as BangumiTypes
import Moe.Domain.Setting.Types (MetadataConfig (..))
import Moe.Domain.Setting.Types qualified as Setting
import Moe.Effect.Metadata (Metadata (..))
import Moe.Effect.Setting (Setting, getSetting)
import Moe.Infra.BangumiData.Client qualified as BangumiDataClient
import Moe.Infra.BangumiData.Types (BangumiDataItem (..), TitleTranslate (..))
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.Tmdb.Client qualified as Tmdb
import Servant.Client (ClientError, mkClientEnv, runClientM)
import Web.Bgmtv.Client qualified as Bgmtv
import Web.Bgmtv.Types qualified as Bgmtv

bgmtvUserAgent :: Text
bgmtvUserAgent = "moe-bangumi/0.1.0"

runMetadataHttp ::
  (IOE :> es, Error MoeError :> es, Setting :> es) =>
  Eff (Metadata : es) a ->
  Eff es a
runMetadataHttp = interpret $ \_ -> \case
  SearchBgmtv keyword maybeYear -> do
    let bgmtvConfig = Bgmtv.defaultConfig bgmtvUserAgent
        req = buildBgmtvRequest keyword maybeYear
    result <- liftIO $ Bgmtv.runBgmtv bgmtvConfig (Bgmtv.searchSubjectsM bgmtvConfig.userAgent req)
    case result of
      Left err -> throwError $ ExternalApiError ("Bgmtv search failed: " <> T.pack (show err))
      Right resp -> pure resp.data_
  SearchTmdb keyword _maybeYear -> do
    pref <- getSetting
    apiKey <- case Setting.metadata pref of
      Just cfg -> pure cfg.tmdbApiKey
      Nothing -> throwError $ ExternalApiError "TMDB API key not configured"
    manager <- liftIO $ newManager tlsManagerSettings
    let clientEnv = mkClientEnv manager Tmdb.tmdbBaseUrl
    result <- liftIO $ runClientM (Tmdb.searchMulti apiKey "zh-CN" keyword) clientEnv
    case result of
      Left err -> throwError $ ExternalApiError ("TMDB search failed: " <> T.pack (show err))
      Right resp -> pure resp.results
  SearchBangumiData keyword maybeYear -> do
    let targetYear = fromMaybe 2024 maybeYear
        months = [1 .. 12]
    result <- liftIO $ BangumiDataClient.fetchByMonths targetYear months
    case result of
      Left err -> throwError $ ExternalApiError ("BangumiData fetch failed: " <> T.pack err)
      Right items -> pure $ filter (matchesKeyword keyword) items
  GetBgmtvDetail bgmtvId -> do
    let bgmtvConfig = Bgmtv.defaultConfig bgmtvUserAgent
    result <- liftIO $ Bgmtv.getSubject bgmtvConfig (fromIntegral bgmtvId)
    pure $ either (const Nothing) Just result
  GetTmdbTvDetail tmdbId -> do
    pref <- getSetting
    case Setting.metadata pref of
      Nothing -> pure Nothing
      Just cfg -> do
        manager <- liftIO $ newManager tlsManagerSettings
        let clientEnv = mkClientEnv manager Tmdb.tmdbBaseUrl
        result <- liftIO $ runClientM (Tmdb.getTvDetail cfg.tmdbApiKey "zh-CN" (fromIntegral tmdbId)) clientEnv
        pure $ handleTmdbResult result
  GetTmdbMovieDetail tmdbId -> do
    pref <- getSetting
    case Setting.metadata pref of
      Nothing -> pure Nothing
      Just cfg -> do
        manager <- liftIO $ newManager tlsManagerSettings
        let clientEnv = mkClientEnv manager Tmdb.tmdbBaseUrl
        result <- liftIO $ runClientM (Tmdb.getMovieDetail cfg.tmdbApiKey "zh-CN" (fromIntegral tmdbId)) clientEnv
        pure $ handleTmdbResult result
  FetchBangumiDataBySeason season -> do
    let year = fromIntegral season.year
        months = BangumiTypes.seasonToMonths season.season
    result <- liftIO $ BangumiDataClient.fetchByMonths year months
    case result of
      Left err -> throwError $ ExternalApiError ("BangumiData fetch failed: " <> T.pack err)
      Right items -> pure items

handleTmdbResult :: Either ClientError a -> Maybe a
handleTmdbResult = either (const Nothing) Just

buildBgmtvRequest :: Text -> Maybe Word16 -> Bgmtv.SearchRequest
buildBgmtvRequest keyword maybeYear =
  Bgmtv.SearchRequest
    { keyword = keyword,
      filter_ =
        Just
          Bgmtv.SearchFilter
            { subjectType = Just [Bgmtv.Anime],
              metaTags = Nothing,
              airDate = fmap buildYearFilter maybeYear
            }
    }

buildYearFilter :: Word16 -> [Text]
buildYearFilter year =
  [ ">=" <> T.pack (show year) <> "-01-01",
    "<=" <> T.pack (show year) <> "-12-31"
  ]

matchesKeyword :: Text -> BangumiDataItem -> Bool
matchesKeyword keyword item =
  let kw = T.toLower keyword
      titleMatch = kw `T.isInfixOf` T.toLower item.title
      TitleTranslate hans hant = item.titleTranslate
      zhHansMatch = any (T.isInfixOf kw . T.toLower) hans
      zhHantMatch = any (T.isInfixOf kw . T.toLower) hant
   in titleMatch || zhHansMatch || zhHantMatch
