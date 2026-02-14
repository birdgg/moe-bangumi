import type { BangumiResponse } from "@/client/types.gen"
import { getWeekdayName } from "../utils/weekday"
import { CalendarCard } from "./calendar-card"

interface WeekdayRowProps {
  weekday: number
  bangumis: BangumiResponse[]
}

export function WeekdayRow({ weekday, bangumis }: WeekdayRowProps) {
  const jsDay = new Date().getDay()
  const isoDay = jsDay === 0 ? 7 : jsDay
  const isToday = isoDay === weekday

  return (
    <div className="flex flex-col gap-3">
      <div className="flex items-center gap-3">
        <div
          className={`shrink-0 rounded-lg px-4 py-2 text-sm font-semibold ${
            isToday
              ? "bg-chart-1 text-white"
              : "bg-muted text-muted-foreground"
          }`}
        >
          {getWeekdayName(weekday)}
        </div>
        <span className="text-xs text-muted-foreground">
          {bangumis.length} 部
        </span>
      </div>
      <div className="grid grid-cols-2 gap-3 sm:grid-cols-3 md:grid-cols-4 lg:grid-cols-5 xl:grid-cols-6 2xl:grid-cols-7 sm:gap-4">
        {bangumis.map((bangumi) => (
          <CalendarCard key={bangumi.id ?? bangumi.titleChs} bangumi={bangumi} />
        ))}
        {bangumis.length === 0 && (
          <div className="col-span-full flex h-72 items-center justify-center rounded-2xl bg-muted/20 text-sm text-muted-foreground backdrop-blur-sm">
            暂无番剧
          </div>
        )}
      </div>
    </div>
  )
}
