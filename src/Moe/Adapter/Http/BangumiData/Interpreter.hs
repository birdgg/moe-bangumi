module Moe.Adapter.Http.BangumiData.Interpreter
  ( runBangumiDataHttp,
  )
where

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static (Error, throwError)
import Moe.Adapter.Http.BangumiData.Client qualified as Client
import Moe.Adapter.Http.BangumiData.Types (BangumiDataItem, itemToAnimeSeason)
import Moe.App.Error (MoeError (..))
import Moe.Core.Bangumi.Types (AnimeSeason (..), seasonToMonths)
import Moe.Effect.BangumiData (BangumiData (..))

runBangumiDataHttp ::
  (IOE :> es, Error MoeError :> es) =>
  Eff (BangumiData : es) a ->
  Eff es a
runBangumiDataHttp = interpret $ \_ -> \case
  FetchBangumiDataByAnimeSeason targetSeason -> do
    let months = seasonToMonths targetSeason.season
    result <- liftIO $ Client.fetchByMonths targetSeason.year months
    case result of
      Left err -> throwError $ ExternalApiError ("Failed to parse bangumi-data: " <> toText err)
      Right items -> pure $ filter (matchesAnimeSeason targetSeason) items

matchesAnimeSeason :: AnimeSeason -> BangumiDataItem -> Bool
matchesAnimeSeason target item =
  case itemToAnimeSeason item of
    Just s -> s == target
    Nothing -> False
