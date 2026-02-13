# Revision history for moe-bangumi

## 1.1.0 -- 2026-02-13

* Import Emby watch progress and auto-subscribe current season RSS
* Replace manual HTTP calls with Servant client for Emby API
* Simplify collection rename by skipping specialDir paths
* Improve RSS error formatting and error handling
* Add SPA fallback for frontend routes
* Handle TestConnection independently of saved downloader config
* Optimize Docker build with BuildKit cache and registry-based caching

## 1.0.0 -- 2026-02-12

* Initial release
* Bangumi subscription and automatic downloading via RSS
* File renaming and organization for media servers (Plex, Emby, Jellyfin)
* Collection and subscription rename strategies
* Calendar sync
* Web API with Servant
* Metadata integration (TMDB, Bangumi.tv, BangumiData)
* Notification support
* qBittorrent downloader integration
