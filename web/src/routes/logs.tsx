import { createFileRoute, useNavigate, useSearch } from "@tanstack/react-router";
import { useQuery } from "@tanstack/react-query";
import { z } from "zod";
import { format, subDays, addDays, isToday, isYesterday } from "date-fns";
import { zhCN } from "date-fns/locale";
import { Calendar } from "@/components/ui/calendar";
import { Button } from "@/components/ui/button";
import { Skeleton } from "@/components/ui/skeleton";
import {
  Popover,
  PopoverContent,
  PopoverTrigger,
} from "@/components/ui/popover";
import {
  IconChevronLeft,
  IconChevronRight,
  IconCalendar,
  IconTerminal2,
  IconPointFilled,
  IconAlertTriangle,
} from "@tabler/icons-react";
import { cn } from "@/lib/utils";
import { motion, AnimatePresence } from "framer-motion";

// -- Types --

interface LogEntry {
  time: string;
  level: string;
  component: string;
  message: string;
}

interface LogsResponse {
  entries: LogEntry[];
  total: number;
  page: number;
  pageSize: number;
}

// -- Route --

const searchSchema = z.object({
  date: z
    .string()
    .optional()
    .default(format(new Date(), "yyyy-MM-dd")),
  page: z.number().optional().default(1),
  pageSize: z.number().optional().default(50),
});

export const Route = createFileRoute("/logs")({
  validateSearch: searchSchema,
  component: LogsPage,
});

// -- Data fetching --

function useLogs(date: string, page: number, pageSize: number) {
  return useQuery<LogsResponse>({
    queryKey: ["logs", date, page, pageSize],
    queryFn: async () => {
      const params = new URLSearchParams({
        date,
        page: String(page),
        pageSize: String(pageSize),
      });
      const res = await fetch(`/api/logs?${params}`);
      if (!res.ok) throw new Error("Failed to fetch logs");
      return res.json();
    },
  });
}

// -- Helpers --

function formatTime(iso: string): string {
  try {
    return format(new Date(iso), "HH:mm:ss");
  } catch {
    return iso;
  }
}

function formatDateLabel(dateStr: string): string {
  const d = new Date(dateStr + "T00:00:00");
  if (isToday(d)) return "Today";
  if (isYesterday(d)) return "Yesterday";
  return format(d, "M/d EEE", { locale: zhCN });
}

function isAttentionLevel(level: string): boolean {
  return level.toLowerCase().includes("attention");
}

// -- Page --

function LogsPage() {
  const { date, page, pageSize } = useSearch({ from: "/logs" });
  const navigate = useNavigate({ from: "/logs" });
  const { data, isLoading, error } = useLogs(date, page, pageSize);

  const totalPages = data ? Math.ceil(data.total / data.pageSize) : 0;
  const currentDate = new Date(date + "T00:00:00");
  const canGoForward = !isToday(currentDate);

  const setDate = (d: string) =>
    navigate({ search: { date: d, page: 1, pageSize } });
  const setPage = (p: number) =>
    navigate({ search: { date, page: p, pageSize } });

  const goToPrevDay = () =>
    setDate(format(subDays(currentDate, 1), "yyyy-MM-dd"));
  const goToNextDay = () => {
    if (canGoForward) {
      setDate(format(addDays(currentDate, 1), "yyyy-MM-dd"));
    }
  };

  const warnCount = data
    ? data.entries.filter((e) => isAttentionLevel(e.level)).length
    : 0;

  return (
    <div className="min-h-full">
      <div className="px-6 py-8 md:px-8">
        <div className="max-w-5xl mx-auto">
          {/* Date navigation bar */}
          <motion.div
            initial={{ opacity: 0, y: -4 }}
            animate={{ opacity: 1, y: 0 }}
            transition={{
              duration: 0.4,
              delay: 0.05,
              ease: [0.33, 1, 0.68, 1],
            }}
            className="flex items-center justify-between mb-4"
          >
            <div className="flex items-center gap-1.5">
              <Button
                variant="ghost"
                size="icon-sm"
                onClick={goToPrevDay}
              >
                <IconChevronLeft className="size-4" />
              </Button>

              <Popover>
                <PopoverTrigger
                  className={cn(
                    "inline-flex items-center gap-2 px-3 h-7 rounded-lg",
                    "text-sm font-medium tabular-nums",
                    "hover:bg-muted/60 transition-colors cursor-pointer"
                  )}
                >
                  <IconCalendar className="size-3.5 text-muted-foreground" />
                  <span>{date}</span>
                  <span className="text-muted-foreground text-xs">
                    {formatDateLabel(date)}
                  </span>
                </PopoverTrigger>
                <PopoverContent align="start" sideOffset={8} className="w-auto p-0">
                  <Calendar
                    mode="single"
                    selected={currentDate}
                    onSelect={(d) => {
                      if (d) setDate(format(d, "yyyy-MM-dd"));
                    }}
                    disabled={{ after: new Date() }}
                  />
                </PopoverContent>
              </Popover>

              <Button
                variant="ghost"
                size="icon-sm"
                onClick={goToNextDay}
                disabled={!canGoForward}
              >
                <IconChevronRight className="size-4" />
              </Button>
            </div>

            {/* Stats pills */}
            {data && warnCount > 0 && (
              <motion.div
                initial={{ opacity: 0 }}
                animate={{ opacity: 1 }}
                transition={{ delay: 0.2 }}
                className="flex items-center gap-3 text-xs text-muted-foreground"
              >
                <span className="inline-flex items-center gap-1 text-destructive">
                  <IconAlertTriangle className="size-3" />
                  {warnCount}
                </span>
              </motion.div>
            )}
          </motion.div>

          {/* Log container */}
          <motion.div
            initial={{ opacity: 0, y: 6 }}
            animate={{ opacity: 1, y: 0 }}
            transition={{
              duration: 0.5,
              delay: 0.1,
              ease: [0.33, 1, 0.68, 1],
            }}
            className="liquid-glass-card rounded-2xl overflow-hidden relative"
          >
            <div className="liquid-glass-refraction" />
            <div className="relative">
              {isLoading ? (
                <LoadingSkeleton />
              ) : error ? (
                <ErrorState error={error} />
              ) : !data || data.entries.length === 0 ? (
                <EmptyState date={date} />
              ) : (
                <LogEntryList entries={data.entries} />
              )}
            </div>
          </motion.div>

          {/* Pagination */}
          <AnimatePresence>
            {data && data.total > data.pageSize && (
              <motion.div
                initial={{ opacity: 0, y: 4 }}
                animate={{ opacity: 1, y: 0 }}
                exit={{ opacity: 0, y: 4 }}
                transition={{ duration: 0.3, delay: 0.15 }}
                className="flex items-center justify-between mt-4 px-1"
              >
                <p className="text-xs text-muted-foreground tabular-nums">
                  {(page - 1) * pageSize + 1}-
                  {Math.min(page * pageSize, data.total)} / {data.total}
                </p>
                <div className="flex items-center gap-1">
                  <Button
                    variant="ghost"
                    size="icon-sm"
                    disabled={page <= 1}
                    onClick={() => setPage(page - 1)}
                  >
                    <IconChevronLeft className="size-3.5" />
                  </Button>
                  <span className="text-xs text-muted-foreground min-w-[4rem] text-center tabular-nums">
                    {page} / {totalPages}
                  </span>
                  <Button
                    variant="ghost"
                    size="icon-sm"
                    disabled={page >= totalPages}
                    onClick={() => setPage(page + 1)}
                  >
                    <IconChevronRight className="size-3.5" />
                  </Button>
                </div>
              </motion.div>
            )}
          </AnimatePresence>
        </div>
      </div>
    </div>
  );
}

// -- Log entry list --

function LogEntryList({ entries }: { entries: LogEntry[] }) {
  return (
    <div className="divide-y divide-border/50">
      {entries.map((entry, i) => (
        <LogRow key={i} entry={entry} index={i} />
      ))}
    </div>
  );
}

function LogRow({ entry, index }: { entry: LogEntry; index: number }) {
  const warn = isAttentionLevel(entry.level);

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
      {/* Time */}
      <span className="text-muted-foreground/60 text-xs tabular-nums select-none">
        {formatTime(entry.time)}
      </span>

      {/* Level indicator + component */}
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

      {/* Message */}
      <span
        className={cn(
          "min-w-0 break-words",
          warn && "text-destructive/90"
        )}
      >
        {entry.message}
      </span>

      {/* Level tag */}
      <span
        className={cn(
          "text-[10px] font-medium uppercase tracking-widest text-right select-none",
          warn ? "text-destructive/60" : "text-muted-foreground/30"
        )}
      >
        {warn ? "WARN" : "INFO"}
      </span>
    </motion.div>
  );
}

// -- States --

function LoadingSkeleton() {
  return (
    <div className="p-4 space-y-2">
      {Array.from({ length: 12 }).map((_, i) => (
        <motion.div
          key={i}
          initial={{ opacity: 0 }}
          animate={{ opacity: 1 }}
          transition={{ delay: i * 0.04 }}
          className="grid grid-cols-[3.5rem_auto_1fr_4.5rem] items-center gap-x-3"
        >
          <Skeleton className="h-3.5 w-12 rounded" />
          <div className="flex items-center gap-1.5">
            <Skeleton className="size-2.5 rounded-full" />
            <Skeleton className="h-3 w-10 rounded" />
          </div>
          <Skeleton className="h-3.5 rounded" style={{ width: `${40 + Math.random() * 50}%` }} />
          <Skeleton className="h-3 w-8 rounded ml-auto" />
        </motion.div>
      ))}
    </div>
  );
}

function EmptyState({ date }: { date: string }) {
  return (
    <div className="flex flex-col items-center justify-center py-20 text-center">
      <motion.div
        initial={{ opacity: 0, scale: 0.9 }}
        animate={{ opacity: 1, scale: 1 }}
        transition={{ duration: 0.4, ease: [0.33, 1, 0.68, 1] }}
      >
        <IconTerminal2 className="size-8 text-muted-foreground/20 mb-3" />
        <p className="text-sm text-muted-foreground/60">
          {date} - no log entries
        </p>
      </motion.div>
    </div>
  );
}

function ErrorState({ error }: { error: Error }) {
  return (
    <div className="flex flex-col items-center justify-center py-20 text-center">
      <motion.div
        initial={{ opacity: 0, scale: 0.9 }}
        animate={{ opacity: 1, scale: 1 }}
        transition={{ duration: 0.4, ease: [0.33, 1, 0.68, 1] }}
      >
        <IconAlertTriangle className="size-8 text-destructive/40 mb-3" />
        <p className="text-sm text-destructive/80">{error.message}</p>
      </motion.div>
    </div>
  );
}
