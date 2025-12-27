import { useEffect, useCallback } from "react";
import { toast } from "sonner";

interface Log {
  id: number;
  created_at: string;
  level: "info" | "warning" | "error";
  message: string;
}

export function useLogStream() {
  const handleLog = useCallback((log: Log) => {
    // Only show toast for warning and error levels
    if (log.level === "error") {
      toast.error(log.message, {
        duration: 8000,
      });
    } else if (log.level === "warning") {
      toast.warning(log.message, {
        duration: 6000,
      });
    }
  }, []);

  useEffect(() => {
    const eventSource = new EventSource("/api/logs/stream");

    eventSource.onmessage = (e) => {
      try {
        const log = JSON.parse(e.data) as Log;
        handleLog(log);
      } catch (err) {
        console.error("Failed to parse SSE log:", err);
      }
    };

    eventSource.onerror = () => {
      console.error("SSE connection error, will retry...");
      eventSource.close();
    };

    return () => {
      eventSource.close();
    };
  }, [handleLog]);
}
