-- | Periodic GC statistics monitor for diagnosing memory growth.
module Moe.App.MemoryMonitor
  ( memoryMonitorThread,
  )
where

import Data.Aeson (object, (.=))
import Effectful.Concurrent.STM qualified as STM
import Effectful.Log qualified as Log
import GHC.Stats (GCDetails (..), RTSStats (..), getRTSStats)
import Moe.App.Env (MoeEnv)
import Moe.Job.Effect (runBaseEffects)
import Moe.Prelude

-- | Entry point for the memory monitor thread.
-- Logs heap statistics every 5 minutes for operational visibility.
memoryMonitorThread :: MoeEnv -> Logger -> IO ()
memoryMonitorThread env logger =
  runBaseEffects env logger "Memory" monitorLoop

monitorLoop :: (Concurrent :> es, Log :> es, IOE :> es) => Eff es ()
monitorLoop = do
  Log.logInfo_ "Memory monitor started"
  void $ infinitely $ do
    timerVar <- STM.registerDelay fiveMinutes
    STM.atomically $ STM.readTVar timerVar >>= STM.check
    stats <- liftIO getRTSStats
    let gcDetails = stats.gc
        liveMB = toMB gcDetails.gcdetails_live_bytes
        maxLiveMB = toMB stats.max_live_bytes
        heapMB = toMB gcDetails.gcdetails_mem_in_use_bytes
        gcCount = stats.gcs
    Log.logInfo "GC stats" $
      object
        [ "live_mb" .= liveMB,
          "max_live_mb" .= maxLiveMB,
          "heap_mb" .= heapMB,
          "gc_count" .= gcCount
        ]

toMB :: Word64 -> Double
toMB bytes = fromIntegral bytes / (1024 * 1024)

fiveMinutes :: Int
fiveMinutes = 5 * 60 * 1_000_000
