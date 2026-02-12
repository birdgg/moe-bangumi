-- | Calendar sync core logic: fetches and stores season data.
module Moe.Job.CalendarSync.Process
  ( runCalendarSync,
  )
where

import Effectful
import Moe.App.Calendar (syncAirSeason)
import Moe.Domain.Bangumi (getCurrentAirSeason)
import Moe.Infra.Metadata.Effect (Metadata)
import Moe.Infra.Setting.Effect (Setting)
import Moe.Prelude

-- | Get the current air season and sync it.
runCalendarSync ::
  (Metadata :> es, Setting :> es, Sqlite :> es, Concurrent :> es, Log :> es, IOE :> es) =>
  Eff es ()
runCalendarSync = do
  season <- liftIO getCurrentAirSeason
  void $ syncAirSeason season
