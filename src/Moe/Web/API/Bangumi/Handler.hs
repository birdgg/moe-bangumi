module Moe.Web.API.Bangumi.Handler
  ( handleBangumiSeason,
  )
where

import Data.Maybe (fromMaybe)
import Data.Time.Calendar (Year)
import Moe.App.CalendarSync (syncAirSeason)
import Moe.Domain.Bangumi.Types (AirSeason (..), Season)
import Moe.Web.API.DTO.Bangumi (BangumiResponse, toBangumiResponse)
import Moe.Web.Types (MoeEff)

handleBangumiSeason :: Year -> Season -> Maybe Bool -> MoeEff [BangumiResponse]
handleBangumiSeason year season mForce = do
  let airSeason = AirSeason year season
      forceRefresh = fromMaybe False mForce
  bangumis <- syncAirSeason forceRefresh airSeason
  pure $ map toBangumiResponse bangumis
