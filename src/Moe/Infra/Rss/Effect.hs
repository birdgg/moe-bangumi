module Moe.Infra.Rss.Effect
  ( Rss,
    fetchRss,
    runRss,
    module Moe.Infra.Rss.Types,
  )
where

import Effectful.TH (makeEffect)
import Moe.Infra.Http.Effect (Http, getHttpManager)
import Moe.Infra.Rss.Client qualified as Client
import Moe.Infra.Rss.Types
import Moe.Prelude

data Rss :: Effect where
  FetchRss :: Text -> Rss m [RawItem]

makeEffect ''Rss

runRss ::
  (Http :> es, IOE :> es, Error RssFetchError :> es) =>
  Eff (Rss : es) a ->
  Eff es a
runRss = interpret $ \_ -> \case
  FetchRss url -> do
    mgr <- getHttpManager
    result <- liftIO $ Client.fetchRss mgr url
    liftEither result
