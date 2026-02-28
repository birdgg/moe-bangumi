export function formatBytes(bytes: number): string {
  if (bytes === 0) return "0 B"
  const units = ["B", "KB", "MB", "GB", "TB"]
  const i = Math.floor(Math.log(bytes) / Math.log(1024))
  const value = bytes / Math.pow(1024, i)
  return `${value.toFixed(i === 0 ? 0 : 1)} ${units[i]}`
}

export function formatSpeed(bytesPerSec: number): string {
  if (bytesPerSec === 0) return "-"
  return `${formatBytes(bytesPerSec)}/s`
}

export function formatEta(seconds: number): string {
  if (seconds <= 0 || seconds >= 8640000) return "-"
  const h = Math.floor(seconds / 3600)
  const m = Math.floor((seconds % 3600) / 60)
  if (h > 0) return `${h}h ${m}m`
  if (m > 0) return `${m}m`
  return `${seconds}s`
}

interface StatusInfo {
  label: string
  variant: "default" | "secondary" | "destructive" | "outline"
}

export function getStatusInfo(state: string): StatusInfo {
  switch (state) {
    case "downloading":
    case "forcedDL":
      return { label: "下载中", variant: "default" }
    case "uploading":
    case "forcedUP":
      return { label: "做种中", variant: "secondary" }
    case "stalledDL":
      return { label: "等待中", variant: "outline" }
    case "stalledUP":
      return { label: "已完成", variant: "secondary" }
    case "pausedDL":
    case "stoppedDL":
      return { label: "已暂停", variant: "outline" }
    case "pausedUP":
    case "stoppedUP":
      return { label: "完成", variant: "secondary" }
    case "queuedDL":
    case "queuedUP":
      return { label: "排队中", variant: "outline" }
    case "checkingDL":
    case "checkingUP":
    case "checkingResumeData":
      return { label: "校验中", variant: "outline" }
    case "metaDL":
      return { label: "获取元数据", variant: "outline" }
    case "allocating":
      return { label: "分配空间", variant: "outline" }
    case "moving":
      return { label: "移动中", variant: "outline" }
    case "error":
    case "missingFiles":
      return { label: "错误", variant: "destructive" }
    case "unknown":
    default:
      return { label: "未知", variant: "outline" }
  }
}

export function isPaused(state: string): boolean {
  return state === "pausedDL" || state === "pausedUP" || state === "stoppedDL" || state === "stoppedUP"
}

export function isActive(state: string): boolean {
  return (
    state === "downloading" ||
    state === "uploading" ||
    state === "forcedDL" ||
    state === "forcedUP" ||
    state === "stalledDL" ||
    state === "stalledUP" ||
    state === "metaDL" ||
    state === "queuedDL" ||
    state === "queuedUP"
  )
}
