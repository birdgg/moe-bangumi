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
