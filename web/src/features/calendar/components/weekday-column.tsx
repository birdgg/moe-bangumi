import type { BangumiResponse } from "@/client/types.gen"
import { getWeekdayName } from "../utils/weekday"
import { BangumiCard } from "./bangumi-card"

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
      <div className="flex flex-wrap gap-4">
        {bangumis.map((bangumi) => (
          <BangumiCard key={bangumi.id ?? bangumi.titleChs} bangumi={bangumi} />
        ))}
        {bangumis.length === 0 && (
          <div className="flex h-72 w-full items-center justify-center rounded-2xl bg-muted/20 text-sm text-muted-foreground backdrop-blur-sm">
            暂无番剧
          </div>
        )}
      </div>
    </div>
  )
}
