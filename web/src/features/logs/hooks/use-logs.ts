import { useInfiniteQuery } from "@tanstack/react-query";

interface Log {
  id: number;
  created_at: string;
  level: "info" | "warning" | "error";
  message: string;
}

interface UseLogsParams {
  level?: string;
  pageSize?: number;
}

const PAGE_SIZE = 50;

async function fetchLogs(params: {
  level?: string;
  limit: number;
  offset: number;
}): Promise<Log[]> {
  const searchParams = new URLSearchParams();
  if (params.level) searchParams.set("level", params.level);
  searchParams.set("limit", params.limit.toString());
  searchParams.set("offset", params.offset.toString());

  const response = await fetch(`/api/logs?${searchParams.toString()}`);

  if (!response.ok) {
    throw new Error("Failed to fetch logs");
  }

  return response.json();
}

export function useLogs(params: UseLogsParams = {}) {
  const pageSize = params.pageSize ?? PAGE_SIZE;

  return useInfiniteQuery({
    queryKey: ["logs", params.level],
    queryFn: ({ pageParam = 0 }) =>
      fetchLogs({
        level: params.level,
        limit: pageSize,
        offset: pageParam,
      }),
    initialPageParam: 0,
    getNextPageParam: (lastPage, allPages) => {
      // If we got fewer items than requested, we've reached the end
      if (lastPage.length < pageSize) {
        return undefined;
      }
      // Return the next offset
      return allPages.length * pageSize;
    },
    refetchInterval: 30000, // Auto-refresh every 30 seconds
  });
}
