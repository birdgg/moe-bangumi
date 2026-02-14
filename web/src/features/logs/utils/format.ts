import { format, isToday, isYesterday } from "date-fns"
import { zhCN } from "date-fns/locale"

export function formatTime(iso: string): string {
  try {
    return format(new Date(iso), "HH:mm:ss")
  } catch {
    return iso
  }
}

export function formatDateLabel(dateStr: string): string {
  const d = new Date(dateStr + "T00:00:00")
  if (isToday(d)) return "Today"
  if (isYesterday(d)) return "Yesterday"
  return format(d, "M/d EEE", { locale: zhCN })
}

export function isAttentionLevel(level: string): boolean {
  return level.toLowerCase().includes("attention")
}
