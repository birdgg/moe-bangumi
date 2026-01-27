module Moe.Model.BangumiData.Interpreter
  ( runBangumiDataHttp,
  )
where

import BangumiData.Client qualified as Client
import BangumiData.Types (BangumiDataItem)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static (Error, throwError)
import Moe.Environment.Config (MoeError (..))
import Moe.Model.Bangumi.Types (AnimeSeason)
import Moe.Model.BangumiData.Query (BangumiData (..))
import Moe.Model.BangumiData.Types (itemToAnimeSeason)

runBangumiDataHttp ::
  (IOE :> es, Error MoeError :> es) =>
  Eff (BangumiData : es) a ->
  Eff es a
runBangumiDataHttp = interpret $ \_ -> \case
  FetchBangumiDataByAnimeSeason targetSeason -> do
    result <- liftIO Client.fetchAll
    case result of
      Left err -> throwError $ ExternalApiError ("Failed to parse bangumi-data: " <> toText err)
      Right allItems -> pure $ filter (matchesAnimeSeason targetSeason) allItems

matchesAnimeSeason :: AnimeSeason -> BangumiDataItem -> Bool
matchesAnimeSeason target item =
  case itemToAnimeSeason item of
    Just s -> s == target
    Nothing -> False
