export interface DownloadItem {
  hash: string
  name: string
  size: number
  progress: number
  state: string
  ratio: number
  dlspeed: number
  upspeed: number
  eta: number
  addedOn: number
  tags: string[]
}

export interface TorrentActionRequest {
  hashes: string[]
  deleteFiles?: boolean
}
