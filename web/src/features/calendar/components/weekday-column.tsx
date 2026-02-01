import type { BangumiResponse } from "@/client/types.gen"
import { getWeekdayName } from "../utils/weekday"
import { BangumiCard } from "./bangumi-card"

interface WeekdayRowProps {
  weekday: number
  bangumis: BangumiResponse[]
}

export function WeekdayRow({ weekday, bangumis }: WeekdayRowProps) {
  const isToday = new Date().getDay() === weekday

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
      <div className="flex gap-4 overflow-x-auto pb-2">
        {bangumis.map((bangumi) => (
          <BangumiCard key={bangumi.id ?? bangumi.titleChs} bangumi={bangumi} />
        ))}
        {bangumis.length === 0 && (
          <div className="flex h-40 w-full items-center justify-center rounded-lg bg-muted/30 text-sm text-muted-foreground">
            暂无番剧
          </div>
        )}
      </div>
    </div>
  )
}
