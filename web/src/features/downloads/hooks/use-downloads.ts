import { useQuery, useMutation, useQueryClient } from "@tanstack/react-query";
import { listTorrentsOptions, deleteTorrentsMutation, type Task } from "@/lib/api";

// Re-export type for components
export type { Task };

// Query key for download tasks
const TORRENTS_KEY = ["torrents"] as const;

// List all torrents from downloader
export function useListTorrents() {
  return useQuery({
    ...listTorrentsOptions(),
    refetchInterval: 60000, // Poll every 1 minute
    staleTime: 30000,
  });
}

// Delete torrents mutation (delete from downloader by id/hash)
export function useDeleteTorrents() {
  const queryClient = useQueryClient();
  return useMutation({
    ...deleteTorrentsMutation(),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: TORRENTS_KEY });
    },
  });
}
