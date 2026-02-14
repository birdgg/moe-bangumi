import { useState } from "react"
import type { BangumiResponse, CalendarEntry } from "@/client/types.gen"
import {
  Select,
  SelectContent,
  SelectItem,
  SelectTrigger,
  SelectValue,
} from "@/components/ui/select"
import { getOrderedWeekdays } from "../utils/weekday"
import { WeekdayRow } from "./weekday-column"
import { type Season, getCurrentSeason, useCalendar } from "../hooks/use-calendar"
import { Skeleton } from "@/components/ui/skeleton"
import { TrackingModal } from "@/features/bangumi/components/tracking-modal"

const SEASONS: { value: Season; label: string }[] = [
  { value: "Winter", label: "冬" },
  { value: "Spring", label: "春" },
  { value: "Summer", label: "夏" },
  { value: "Fall", label: "秋" },
]

const START_YEAR = 2013

function getAirSeasonOptions() {
  const current = getCurrentSeason()
  const options: { year: number; season: Season }[] = []

  for (let year = current.year; year >= START_YEAR; year--) {
    const seasons = year === current.year
      ? SEASONS.filter((s) => SEASONS.indexOf(s) <= SEASONS.findIndex((s) => s.value === current.season))
      : SEASONS
    for (const s of seasons) {
      options.push({ year, season: s.value })
    }
  }

  return options
}

function formatAirSeason(year: number, season: Season): string {
  const label = SEASONS.find((s) => s.value === season)?.label ?? season
  return `${year}-${label}`
}

function CalendarSkeleton() {
  return (
    <div className="flex flex-col gap-6">
      {Array.from({ length: 7 }).map((_, i) => (
        <div key={i} className="flex flex-col gap-3">
          <Skeleton className="h-9 w-20 rounded-lg" />
          <div className="grid grid-cols-2 gap-3 sm:grid-cols-3 md:grid-cols-4 lg:grid-cols-5 xl:grid-cols-6 2xl:grid-cols-7 sm:gap-4">
            {Array.from({ length: 5 }).map((_, j) => (
              <Skeleton key={j} className="aspect-[2/3] w-full rounded-xl" />
            ))}
          </div>
        </div>
      ))}
    </div>
  )
}

export function CalendarView() {
  const current = getCurrentSeason()
  const [year, setYear] = useState(current.year)
  const [season, setSeason] = useState<Season>(current.season)
  const { data: calendar, isLoading, error } = useCalendar({ year, season })

  const airSeasonOptions = getAirSeasonOptions()

  const selectedValue = formatAirSeason(year, season)

  const labelToSeason = new Map(SEASONS.map((s) => [s.label, s.value]))

  const handleSeasonChange = (value: string | null) => {
    if (!value) return
    const [y, label] = value.split("-")
    setYear(Number(y))
    setSeason(labelToSeason.get(label) ?? season)
  }

  return (
    <div className="flex flex-col gap-6">
      <Select value={selectedValue} onValueChange={handleSeasonChange}>
        <SelectTrigger className="ml-auto w-auto">
          <SelectValue placeholder="选择季度" />
        </SelectTrigger>
        <SelectContent>
          {airSeasonOptions.map((opt) => {
            const val = formatAirSeason(opt.year, opt.season)
            return (
              <SelectItem key={val} value={val}>
                {formatAirSeason(opt.year, opt.season)}
              </SelectItem>
            )
          })}
        </SelectContent>
      </Select>

      {isLoading && <CalendarSkeleton />}

      {error && (
        <div className="rounded-lg bg-destructive/10 p-4 text-center text-destructive">
          加载失败: {error.message}
        </div>
      )}

      {calendar && <CalendarContent entries={calendar} />}
    </div>
  )
}

function CalendarContent({ entries }: { entries: CalendarEntry[] }) {
  const [selectedBangumi, setSelectedBangumi] = useState<BangumiResponse | null>(null)

  const calendarMap = new Map<number, CalendarEntry>()
  entries.forEach((entry) => {
    calendarMap.set(entry.weekday, entry)
  })

  const orderedWeekdays = getOrderedWeekdays()

  return (
    <>
      {orderedWeekdays.map((weekday) => {
        const entry = calendarMap.get(weekday)
        return (
          <WeekdayRow
            key={weekday}
            weekday={weekday}
            bangumis={entry?.bangumis ?? []}
            onSelectBangumi={setSelectedBangumi}
          />
        )
      })}

      {selectedBangumi && (
        <TrackingModal
          open={!!selectedBangumi}
          onOpenChange={(open) => {
            if (!open) setSelectedBangumi(null)
          }}
          bangumi={selectedBangumi}
        />
      )}
    </>
  )
}
