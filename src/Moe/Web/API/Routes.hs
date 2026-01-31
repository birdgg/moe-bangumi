module Moe.Web.API.Routes where

import Data.Text (Text)
import Data.Time.Calendar (Year)
import GHC.Generics (Generic)
import Moe.Domain.Bangumi.Types (Season)
import Moe.Web.API.Types (BangumiResponse)
import Servant

type Routes = "api" :> NamedRoutes Routes'

data Routes' mode = Routes'
  { health :: mode :- "health" :> Get '[JSON] Text,
    bangumiSeason ::
      mode
        :- "bangumi"
          :> "season"
          :> QueryParam' '[Required, Strict] "year" Year
          :> QueryParam' '[Required, Strict] "season" Season
          :> Get '[JSON] [BangumiResponse]
  }
  deriving stock (Generic)
