const WEEKDAY_NAMES = ["周日", "周一", "周二", "周三", "周四", "周五", "周六"] as const

export function getWeekdayName(weekday: number): string {
  return WEEKDAY_NAMES[weekday] ?? `星期${weekday}`
}

export function getOrderedWeekdays(): number[] {
  return [1, 2, 3, 4, 5, 6, 0]
}
