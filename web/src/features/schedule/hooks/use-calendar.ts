import { useQuery } from "@tanstack/react-query";
import { getCalendarOptions } from "@/lib/api";

export function useCalendar() {
  return useQuery({
    ...getCalendarOptions(),
  });
}
