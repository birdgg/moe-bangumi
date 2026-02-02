module Moe.Web.API.Bangumi.Handler
  ( handleBangumiSeason,
  )
where

import Data.Maybe (fromMaybe)
import Data.Time.Calendar (Year)
import Moe.App.CalendarSync (syncBangumiSeason)
import Moe.Domain.Bangumi.Types (BangumiSeason (..), Season)
import Moe.Web.API.DTO.Bangumi (BangumiResponse, toBangumiResponse)
import Moe.Web.Types (MoeEff)

handleBangumiSeason :: Year -> Season -> Maybe Bool -> MoeEff [BangumiResponse]
handleBangumiSeason year season mForce = do
  let bs = BangumiSeason year season
      forceRefresh = fromMaybe False mForce
  bangumis <- syncBangumiSeason forceRefresh bs
  pure $ map toBangumiResponse bangumis
