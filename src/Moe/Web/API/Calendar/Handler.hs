module Moe.Web.API.Calendar.Handler
  ( handleCalendar,
  )
where

import Data.Map.Strict qualified as Map
import Data.Time.Calendar (Day, Year)
import Data.Time.Calendar.WeekDate (toWeekDate)
import Effectful.Sqlite (notransact)
import Moe.Domain.Bangumi (AirSeason (..), Bangumi (..), Season)
import Moe.Domain.Shared.Entity (Entity (..))
import Moe.Infra.Database.Bangumi qualified as DB
import Moe.App.Calendar (syncAirSeason)
import Moe.Prelude
import Moe.Web.API.DTO.Bangumi (toBangumiResponse)
import Moe.Web.API.DTO.Calendar (CalendarEntry (..))
import Moe.Web.Types (ServerEff)

-- | Return calendar entries for a season, fetching from metadata if not cached.
handleCalendar :: Year -> Season -> ServerEff [CalendarEntry]
handleCalendar year season = do
  let airSeason = AirSeason year season
  cached <- notransact $ DB.listBangumiBySeason airSeason
  bangumis <- case cached of
    [] -> syncAirSeason airSeason
    _ -> pure cached
  pure $ groupByWeekday bangumis

groupByWeekday :: [Entity Bangumi] -> [CalendarEntry]
groupByWeekday bangumis =
  let withWeekday = map (\b -> (getWeekdayNum b.entityVal.airDate, b)) bangumis
      grouped = Map.fromListWith (++) [(w, [b]) | (w, b) <- withWeekday]
   in sortOn (.weekday) [CalendarEntry w (map toBangumiResponse bs) | (w, bs) <- Map.toList grouped]
  where
    getWeekdayNum :: Day -> Int
    getWeekdayNum day =
      let (_, _, wd) = toWeekDate day
       in wd
