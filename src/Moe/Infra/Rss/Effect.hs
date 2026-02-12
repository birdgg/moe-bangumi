module Moe.Infra.Rss.Effect
  ( Rss,
    fetchRss,
    runRss,
    module Moe.Infra.Rss.Types,
  )
where

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.TH (makeEffect)
import Moe.Error (AppError (..))
import Moe.Infra.Rss.Client qualified as Client
import Moe.Infra.Rss.Types
import Moe.Prelude
import Network.HTTP.Client (Manager)

data Rss :: Effect where
  FetchRss :: Text -> Rss m [RawItem]

makeEffect ''Rss

runRss ::
  (IOE :> es, Error AppError :> es) =>
  Manager ->
  Eff (Rss : es) a ->
  Eff es a
runRss manager = interpret $ \_ -> \case
  FetchRss url -> do
    result <- liftIO $ Client.fetchRss manager url
    liftEitherWith RssError result
