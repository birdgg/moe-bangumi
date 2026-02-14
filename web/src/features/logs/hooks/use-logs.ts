import { useQuery } from "@tanstack/react-query"
import type { LogsResponse } from "../types"

export function useLogs(date: string, page: number, pageSize: number) {
  return useQuery<LogsResponse>({
    queryKey: ["logs", date, page, pageSize],
    queryFn: async () => {
      const params = new URLSearchParams({
        date,
        page: String(page),
        pageSize: String(pageSize),
      })
      const res = await fetch(`/api/logs?${params}`)
      if (!res.ok) throw new Error("Failed to fetch logs")
      return res.json()
    },
  })
}
