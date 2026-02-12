const WEEKDAY_NAMES: Record<number, string> = {
  1: "周一",
  2: "周二",
  3: "周三",
  4: "周四",
  5: "周五",
  6: "周六",
  7: "周日",
}

export function getWeekdayName(weekday: number): string {
  return WEEKDAY_NAMES[weekday] ?? `星期${weekday}`
}

/** ISO 8601 weekday order: Mon(1)..Sun(7) */
export function getOrderedWeekdays(): number[] {
  return [1, 2, 3, 4, 5, 6, 7]
}
