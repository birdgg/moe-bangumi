export interface LogEntry {
  time: string
  level: string
  component: string
  message: string
}

export interface LogsResponse {
  entries: LogEntry[]
  total: number
  page: number
  pageSize: number
}
