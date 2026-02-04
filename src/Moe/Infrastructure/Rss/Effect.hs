module Moe.Infrastructure.Rss.Effect
  ( Rss,
    fetchRss,
    runRss,
    module Moe.Infrastructure.Rss.Types,
  )
where

import Effectful
import Moe.Prelude
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static (Error, throwError)
import Moe.Infrastructure.Rss.Client qualified as Client
import Moe.Infrastructure.Rss.Types
import Network.HTTP.Client (Manager)

data Rss :: Effect where
  FetchRss :: Error RssError :> es => Text -> Rss (Eff es) [RawItem]

type instance DispatchOf Rss = Dynamic

fetchRss :: (Rss :> es, Error RssError :> es) => Text -> Eff es [RawItem]
fetchRss url = send (FetchRss url)

runRss ::
  (IOE :> es) =>
  Manager ->
  Eff (Rss : es) a ->
  Eff es a
runRss manager = interpret $ \env -> \case
  FetchRss url -> do
    result <- liftIO $ Client.fetchRss manager url
    case result of
      Left err -> localSeqUnlift env $ \unlift -> unlift $ throwError err
      Right items -> pure items
