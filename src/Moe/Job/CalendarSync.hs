-- | Calendar sync job: dedicated worker thread for periodic season data synchronization.
module Moe.Job.CalendarSync
  ( calendarSyncWorkerThread,
  )
where

import Moe.Job.CalendarSync.Worker (calendarSyncWorkerThread)
