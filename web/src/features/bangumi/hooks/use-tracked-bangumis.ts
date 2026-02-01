import { useQuery } from "@tanstack/react-query";
import { getApiTrackingBangumisOptions } from "@/client/@tanstack/react-query.gen";

export function useTrackedBangumis() {
  return useQuery(getApiTrackingBangumisOptions());
}
