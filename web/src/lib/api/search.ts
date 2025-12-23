import { client } from "./client";
import type { SearchSubjectsResponse } from "./types";

export const searchApi = {
  bangumi: (query: string) =>
    client.get<SearchSubjectsResponse>(
      `/search?bangumi=${encodeURIComponent(query)}`
    ),
};
