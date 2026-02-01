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
import Moe.Adapter.Database.Bangumi qualified as DB
import Moe.Domain.Bangumi.Types (Bangumi (..), BangumiSeason (..), Season, bangumiSeasonToText)
import Moe.Web.API.Bangumi.Types (toBangumiResponse)
import Moe.Web.API.Calendar.Types (CalendarEntry (..), CalendarResponse (..))
import Moe.Web.Types (MoeEff)

handleCalendar :: Year -> Season -> MoeEff CalendarResponse
handleCalendar year season = do
  let bs = BangumiSeason year season
  bangumis <- notransact $ DB.listBangumiBySeason bs
  let entries = groupByWeekday bangumis
  pure CalendarResponse {season = bangumiSeasonToText bs, entries}

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
