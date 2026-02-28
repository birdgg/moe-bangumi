import { useMutation, useQuery, useQueryClient } from "@tanstack/react-query"
import type { DownloadItem, TorrentActionRequest } from "../types"

export function useDownloads() {
  return useQuery<DownloadItem[]>({
    queryKey: ["downloads"],
    queryFn: async () => {
      const res = await fetch("/api/download")
      if (!res.ok) throw new Error("Failed to fetch downloads")
      return res.json()
    },
    refetchInterval: 3000,
  })
}

function useTorrentAction(endpoint: string) {
  const queryClient = useQueryClient()
  return useMutation({
    mutationFn: async (req: TorrentActionRequest) => {
      const res = await fetch(`/api/download/${endpoint}`, {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify(req),
      })
      if (!res.ok) throw new Error(`Failed to ${endpoint} downloads`)
    },
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ["downloads"] })
    },
  })
}

export function usePauseDownloads() {
  return useTorrentAction("pause")
}

export function useResumeDownloads() {
  return useTorrentAction("resume")
}

export function useDeleteDownloads() {
  return useTorrentAction("delete")
}
