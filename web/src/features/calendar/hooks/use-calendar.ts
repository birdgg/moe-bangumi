import { useQuery } from "@tanstack/react-query"
import { getApiCalendarOptions } from "@/client/@tanstack/react-query.gen"

export type Season = "Winter" | "Spring" | "Summer" | "Fall"

interface SeasonParams {
  year: number
  season: Season
}

export function getCurrentSeason(): SeasonParams {
  const now = new Date()
  const month = now.getMonth() + 1
  const year = now.getFullYear()

  if (month >= 1 && month <= 3) {
    return { year, season: "Winter" }
  }
  if (month >= 4 && month <= 6) {
    return { year, season: "Spring" }
  }
  if (month >= 7 && month <= 9) {
    return { year, season: "Summer" }
  }
  return { year, season: "Fall" }
}

export function useCalendar(params?: Partial<SeasonParams>) {
  const currentSeason = getCurrentSeason()
  const year = params?.year ?? currentSeason.year
  const season = params?.season ?? currentSeason.season

  return useQuery({
    ...getApiCalendarOptions({
      query: { year, season },
    }),
  })
}
