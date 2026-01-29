module Moe.Adapter.Http.Metadata
  ( runMetadataHttp,
  )
where

import Data.Text qualified as T
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static (Error, throwError)
import Moe.App.Error (MoeError (..))
import Moe.Domain.Setting.Types (MetadataConfig (..))
import Moe.Domain.Setting.Types qualified as Setting
import Moe.Effect.Metadata (Metadata (..))
import Moe.Effect.Setting (Setting, getSetting)
import Moe.Infra.BangumiData.Client qualified as BangumiDataClient
import Moe.Infra.BangumiData.Types (BangumiDataItem (..), TitleTranslate (..))
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.Tmdb.Client qualified as Tmdb
import Servant.Client (mkClientEnv, runClientM)
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
      Left err -> throwError $ ExternalApiError ("Bgmtv search failed: " <> show err)
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
      Left err -> throwError $ ExternalApiError ("TMDB search failed: " <> show err)
      Right resp -> pure resp.results
  SearchBangumiData keyword maybeYear -> do
    let targetYear = fromMaybe 2024 maybeYear
        months = [1 .. 12]
    result <- liftIO $ BangumiDataClient.fetchByMonths targetYear months
    case result of
      Left err -> throwError $ ExternalApiError ("BangumiData fetch failed: " <> toText err)
      Right items -> pure $ filter (matchesKeyword keyword) items

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
  [ ">=" <> show year <> "-01-01",
    "<=" <> show year <> "-12-31"
  ]

matchesKeyword :: Text -> BangumiDataItem -> Bool
matchesKeyword keyword item =
  let kw = T.toLower keyword
      titleMatch = kw `T.isInfixOf` T.toLower item.title
      TitleTranslate hans hant = item.titleTranslate
      zhHansMatch = any (T.isInfixOf kw . T.toLower) hans
      zhHantMatch = any (T.isInfixOf kw . T.toLower) hant
   in titleMatch || zhHansMatch || zhHantMatch
