module Moe.Adapter.Http.Metadata.Interpreter
  ( runMetadataHttp,
  )
where

import Data.Text qualified as T
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static (Error, throwError)
import Moe.Adapter.Http.BangumiData.Client qualified as BangumiDataClient
import Moe.Adapter.Http.BangumiData.Types (BangumiDataItem (..), TitleTranslate (..))
import Moe.App.Env (MetadataConfig (..))
import Moe.App.Error (MoeError (..))
import Moe.Effect.Metadata (Metadata (..))
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.Tmdb.Client qualified as Tmdb
import Servant.Client (mkClientEnv, runClientM)
import Web.Bgmtv.Client qualified as Bgmtv
import Web.Bgmtv.Types qualified as Bgmtv

runMetadataHttp ::
  (IOE :> es, Error MoeError :> es) =>
  MetadataConfig ->
  Eff (Metadata : es) a ->
  Eff es a
runMetadataHttp config = interpret $ \_ -> \case
  SearchBgmtv keyword maybeYear -> do
    let bgmtvConfig = Bgmtv.defaultConfig config.bgmtvUserAgent
        req = buildBgmtvRequest keyword maybeYear
    result <- liftIO $ Bgmtv.runBgmtv bgmtvConfig (Bgmtv.searchSubjectsM bgmtvConfig.userAgent req)
    case result of
      Left err -> throwError $ ExternalApiError ("Bgmtv search failed: " <> show err)
      Right resp -> pure resp.data_
  SearchTmdb keyword _maybeYear -> do
    manager <- liftIO $ newManager tlsManagerSettings
    let clientEnv = mkClientEnv manager Tmdb.tmdbBaseUrl
    result <- liftIO $ runClientM (Tmdb.searchMulti config.tmdbApiKey "zh-CN" keyword) clientEnv
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
