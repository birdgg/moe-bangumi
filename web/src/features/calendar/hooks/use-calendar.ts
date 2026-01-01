import { useQuery, useMutation, useQueryClient } from "@tanstack/react-query";
import {
  getCalendarOptions,
  getCalendarQueryKey,
  refreshCalendarMutation,
  type Season,
} from "@/lib/api";
import { toast } from "sonner";

export interface CalendarParams {
  year?: number;
  season?: Season;
}

export function useCalendar(params?: CalendarParams) {
  return useQuery({
    ...getCalendarOptions({
      query: {
        year: params?.year,
        season: params?.season,
      },
    }),
  });
}

export function useRefreshCalendar(params?: CalendarParams) {
  const queryClient = useQueryClient();
  return useMutation({
    ...refreshCalendarMutation({
      query: {
        year: params?.year,
        season: params?.season,
      },
    }),
    onSuccess: (data) => {
      // Update the calendar query cache with the new data
      queryClient.setQueryData(
        getCalendarQueryKey({
          query: {
            year: params?.year,
            season: params?.season,
          },
        }),
        data
      );
      toast.success("日历数据已刷新");
    },
    onError: () => {
      toast.error("刷新失败，请稍后重试");
    },
  });
}
