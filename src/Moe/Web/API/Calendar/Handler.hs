module Moe.Web.API.Calendar.Handler
  ( handleCalendar,
  )
where

import Data.List (sortOn)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Time.Calendar (Day, Year)
import Data.Time.Calendar.WeekDate (toWeekDate)
import Effectful.Sqlite (notransact)
import Moe.Infrastructure.Database.Bangumi qualified as DB
import Moe.Domain.Bangumi.Types (Bangumi (..), BangumiSeason (..), Season)
import Moe.Web.API.DTO.Bangumi (toBangumiResponse)
import Moe.Web.API.DTO.Calendar (CalendarEntry (..))
import Moe.Web.Types (MoeEff)

handleCalendar :: Year -> Season -> MoeEff [CalendarEntry]
handleCalendar year season = do
  let bs = BangumiSeason year season
  bangumis <- notransact $ DB.listBangumiBySeason bs
  pure $ groupByWeekday bangumis

groupByWeekday :: [Bangumi] -> [CalendarEntry]
groupByWeekday bangumis =
  let withWeekday = mapMaybe (\b -> (,b) . getWeekdayNum <$> b.airDate) bangumis
      grouped = Map.fromListWith (++) [(w, [b]) | (w, b) <- withWeekday]
   in sortOn (.weekday) [CalendarEntry w (map toBangumiResponse bs) | (w, bs) <- Map.toList grouped]
  where
    getWeekdayNum :: Day -> Int
    getWeekdayNum day =
      let (_, _, wd) = toWeekDate day
       in wd
