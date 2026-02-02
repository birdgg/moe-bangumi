module Moe.Infrastructure.Rss.Effect
  ( Rss,
    fetchRss,
    runRss,
    module Moe.Infrastructure.Rss.Types,
  )
where

import Data.Text (Text)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.TH
import Moe.Infrastructure.Rss.Client qualified as Client
import Moe.Infrastructure.Rss.Types
import Network.HTTP.Client (Manager)

data Rss :: Effect where
  FetchRss :: Text -> Rss m (Either RssError [RawItem])

type instance DispatchOf Rss = Dynamic

makeEffect ''Rss

runRss ::
  (IOE :> es) =>
  Manager ->
  Eff (Rss : es) a ->
  Eff es a
runRss manager = interpret $ \_ -> \case
  FetchRss url -> liftIO $ Client.fetchRss manager url
