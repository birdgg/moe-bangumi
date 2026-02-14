import { useNavigate, useSearch } from "@tanstack/react-router"
import { motion } from "framer-motion"
import { useLogs } from "../hooks/use-logs"
import { isAttentionLevel } from "../utils/format"
import { DateNavigation } from "../components/date-navigation"
import { LogEntryList } from "../components/log-entry-list"
import { LoadingSkeleton } from "../components/loading-skeleton"
import { EmptyState } from "../components/empty-state"
import { ErrorState } from "../components/error-state"
import { Pagination } from "../components/pagination"

export function LogsPage() {
  const { date, page, pageSize } = useSearch({ from: "/logs" })
  const navigate = useNavigate({ from: "/logs" })
  const { data, isLoading, error } = useLogs(date, page, pageSize)

  const totalPages = data ? Math.ceil(data.total / data.pageSize) : 0

  const setDate = (d: string) =>
    navigate({ search: { date: d, page: 1, pageSize } })
  const setPage = (p: number) =>
    navigate({ search: { date, page: p, pageSize } })

  const warnCount = data
    ? data.entries.filter((e) => isAttentionLevel(e.level)).length
    : 0

  return (
    <div className="min-h-full">
      <div className="px-6 py-8 md:px-8">
        <div className="max-w-5xl mx-auto">
          <DateNavigation
            date={date}
            onDateChange={setDate}
            warnCount={warnCount}
          />

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

          {data && (
            <Pagination
              page={page}
              totalPages={totalPages}
              pageSize={data.pageSize}
              total={data.total}
              onPageChange={setPage}
            />
          )}
        </div>
      </div>
    </div>
  )
}
