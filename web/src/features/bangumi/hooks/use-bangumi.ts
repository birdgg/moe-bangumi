import { useQuery, useMutation, useQueryClient } from "@tanstack/react-query";
import {
  getBangumiOptions,
  getBangumiQueryKey,
  getBangumiByIdOptions,
  getBangumiByIdQueryKey,
  searchMetadataOptions,
  searchMikanOptions,
  getMikanRssOptions,
  getEpisodesOptions,
  createBangumiMutation,
  updateBangumiMutation,
  updateMetadataMutation,
  type MetadataSource,
} from "@/lib/api";

// Get all bangumi
export function useGetAllBangumi() {
  return useQuery({
    ...getBangumiOptions(),
  });
}

// Unified metadata search hook
export function useSearchMetadata(keyword: string, source: MetadataSource) {
  return useQuery({
    ...searchMetadataOptions({ query: { keyword, source } }),
    enabled: keyword.length > 0,
  });
}

// Search bangumi from BGM.tv (convenience wrapper)
export function useSearchBangumi(keyword: string) {
  return useSearchMetadata(keyword, "bgmtv");
}

// Search anime from TMDB (convenience wrapper)
export function useSearchTmdb(keyword: string) {
  return useSearchMetadata(keyword, "tmdb");
}

// Get episodes by subject ID
export function useEpisodes(subjectId: number) {
  return useQuery({
    ...getEpisodesOptions({ path: { subject_id: subjectId } }),
    enabled: !!subjectId,
  });
}

// Create a new bangumi
export function useCreateBangumi() {
  const queryClient = useQueryClient();
  return useMutation({
    ...createBangumiMutation(),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: getBangumiQueryKey() });
    },
  });
}

// Search bangumi from Mikan
export function useSearchMikan(keyword: string) {
  return useQuery({
    ...searchMikanOptions({ query: { keyword } }),
    enabled: keyword.length > 0,
  });
}

// Get Mikan bangumi detail with RSS URLs
export function useMikanRss(id: string) {
  return useQuery({
    ...getMikanRssOptions({ query: { id } }),
    enabled: id.length > 0,
  });
}

// Get a single bangumi by ID with RSS subscriptions
export function useGetBangumiById(id: number) {
  return useQuery({
    ...getBangumiByIdOptions({ path: { id } }),
    enabled: !!id,
  });
}

// Update a bangumi
export function useUpdateBangumi() {
  const queryClient = useQueryClient();
  return useMutation({
    ...updateBangumiMutation(),
    onSuccess: (_data, variables) => {
      // Invalidate the list query
      queryClient.invalidateQueries({ queryKey: getBangumiQueryKey() });
      // Invalidate the specific bangumi detail query
      queryClient.invalidateQueries({
        queryKey: getBangumiByIdQueryKey({ path: { id: variables.path.id } }),
      });
    },
  });
}

// Update metadata (for updating mikan_id, etc.)
export function useUpdateMetadata() {
  const queryClient = useQueryClient();
  return useMutation({
    ...updateMetadataMutation(),
    onSuccess: () => {
      // Invalidate bangumi list query since metadata changed
      queryClient.invalidateQueries({ queryKey: getBangumiQueryKey() });
      // Invalidate all bangumi detail queries since we don't know which bangumi uses this metadata
      queryClient.invalidateQueries({
        predicate: (query) => {
          const key = query.queryKey;
          return Array.isArray(key) && key.length > 0 &&
            typeof key[0] === 'object' && key[0] !== null &&
            'url' in key[0] && typeof key[0].url === 'string' &&
            key[0].url.includes('/api/bangumi/');
        },
      });
    },
  });
}
