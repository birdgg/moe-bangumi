import { client } from "./client";
import type { Bangumi, CreateBangumiRequest, UpdateBangumiRequest } from "./types";

export const bangumiApi = {
  list: () => client.get<Bangumi[]>("/bangumi"),

  get: (id: string | number) => client.get<Bangumi>(`/bangumi/${id}`),

  create: (data: CreateBangumiRequest) =>
    client.post<Bangumi>("/bangumi", data),

  update: (id: string | number, data: UpdateBangumiRequest) =>
    client.put<Bangumi>(`/bangumi/${id}`, data),

  delete: (id: string | number) => client.delete<void>(`/bangumi/${id}`),
};
