import { useQuery, useMutation, useQueryClient } from "@tanstack/react-query";
import {
  getBangumiOptions,
  getBangumiQueryKey,
  getBangumiByIdOptions,
  getBangumiByIdQueryKey,
  searchBgmtvOptions,
  searchTmdbOptions,
  searchMikanOptions,
  getMikanRssOptions,
  getEpisodesOptions,
  createBangumiMutation,
  updateBangumiMutation,
} from "@/lib/api";

// Get all bangumi
export function useGetAllBangumi() {
  return useQuery({
    ...getBangumiOptions(),
  });
}

// Search bangumi from BGM.tv
export function useSearchBangumi(keyword: string) {
  return useQuery({
    ...searchBgmtvOptions({ query: { keyword } }),
    enabled: keyword.length > 0,
  });
}

// Search anime from TMDB
export function useSearchTmdb(keyword: string) {
  return useQuery({
    ...searchTmdbOptions({ query: { keyword } }),
    enabled: keyword.length > 0,
  });
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
