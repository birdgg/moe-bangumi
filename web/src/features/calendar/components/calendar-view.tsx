import type { CalendarEntry } from "@/client/types.gen"
import { getOrderedWeekdays } from "../utils/weekday"
import { WeekdayRow } from "./weekday-column"
import { useCalendar } from "../hooks/use-calendar"
import { Skeleton } from "@/components/ui/skeleton"

function CalendarSkeleton() {
  return (
    <div className="flex flex-col gap-6">
      {Array.from({ length: 7 }).map((_, i) => (
        <div key={i} className="flex flex-col gap-3">
          <Skeleton className="h-9 w-20 rounded-lg" />
          <div className="flex gap-4">
            {Array.from({ length: 5 }).map((_, j) => (
              <Skeleton key={j} className="h-56 w-32 shrink-0 rounded-lg" />
            ))}
          </div>
        </div>
      ))}
    </div>
  )
}

export function CalendarView() {
  const { data: calendar, isLoading, error } = useCalendar()

  if (isLoading) {
    return <CalendarSkeleton />
  }

  if (error) {
    return (
      <div className="rounded-lg bg-destructive/10 p-4 text-center text-destructive">
        加载失败: {error.message}
      </div>
    )
  }

  const calendarMap = new Map<number, CalendarEntry>()
  calendar?.forEach((entry) => {
    calendarMap.set(entry.weekday, entry)
  })

  const orderedWeekdays = getOrderedWeekdays()

  return (
    <div className="flex flex-col gap-6">
      {orderedWeekdays.map((weekday) => {
        const entry = calendarMap.get(weekday)
        return (
          <WeekdayRow
            key={weekday}
            weekday={weekday}
            bangumis={entry?.bangumis ?? []}
          />
        )
      })}
    </div>
  )
}
