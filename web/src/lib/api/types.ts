// Bangumi types
export interface Bangumi {
  id: string | number;
  chineseName: string;
  japaneseName: string;
  season: string;
  episodes: number;
  currentEpisode?: number;
  poster: string;
  airDate: string;
  isComplete: boolean;
}

export interface CreateBangumiRequest {
  chineseName: string;
  japaneseName: string;
  season: string;
  episodes: number;
  currentEpisode?: number;
  poster: string;
  airDate: string;
  isComplete: boolean;
}

export interface UpdateBangumiRequest {
  chineseName?: string;
  japaneseName?: string;
  season?: string;
  episodes?: number;
  currentEpisode?: number;
  poster?: string;
  airDate?: string;
  isComplete?: boolean;
}

// BGM.tv Search types
export type Platform = "TV" | "Web" | "DLC" | "剧场版" | "Unknown";

export interface Subject {
  id: number;
  name: string;
  name_cn: string;
  date: string | null;
  platform: Platform | null;
  image: string | null;
  eps: number;
}

export interface SearchSubjectsResponse {
  total: number;
  limit: number;
  offset: number;
  data: Subject[];
}
