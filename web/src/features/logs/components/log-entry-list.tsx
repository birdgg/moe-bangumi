import { cn } from "@/lib/utils"
import { IconPointFilled } from "@tabler/icons-react"
import { motion } from "framer-motion"
import type { LogEntry } from "../types"
import { formatTime, isAttentionLevel } from "../utils/format"

export function LogEntryList({ entries }: { entries: LogEntry[] }) {
  return (
    <div className="divide-y divide-border/50">
      {entries.map((entry, i) => (
        <LogRow key={i} entry={entry} index={i} />
      ))}
    </div>
  )
}

function LogRow({ entry, index }: { entry: LogEntry; index: number }) {
  const warn = isAttentionLevel(entry.level)

  return (
    <motion.div
      initial={{ opacity: 0 }}
      animate={{ opacity: 1 }}
      transition={{
        duration: 0.25,
        delay: Math.min(index * 0.02, 0.6),
        ease: "easeOut",
      }}
      className={cn(
        "group grid grid-cols-[3.5rem_auto_1fr_4.5rem] items-baseline gap-x-3",
        "px-4 py-2 text-[13px] leading-relaxed",
        "hover:bg-foreground/[0.03] transition-colors duration-150",
        warn && "bg-destructive/[0.03]"
      )}
    >
      <span className="text-muted-foreground/60 text-xs tabular-nums select-none">
        {formatTime(entry.time)}
      </span>

      <span className="inline-flex items-center gap-1.5 shrink-0">
        <IconPointFilled
          className={cn(
            "size-2.5 shrink-0",
            warn ? "text-destructive" : "text-chart-1/70"
          )}
        />
        <span
          className={cn(
            "text-xs tracking-wide uppercase select-none",
            warn
              ? "text-destructive/80 font-medium"
              : "text-muted-foreground/50"
          )}
        >
          {entry.component}
        </span>
      </span>

      <span
        className={cn(
          "min-w-0 break-words",
          warn && "text-destructive/90"
        )}
      >
        {entry.message}
      </span>

      <span
        className={cn(
          "text-[10px] font-medium uppercase tracking-widest text-right select-none",
          warn ? "text-destructive/60" : "text-muted-foreground/30"
        )}
      >
        {warn ? "WARN" : "INFO"}
      </span>
    </motion.div>
  )
}
