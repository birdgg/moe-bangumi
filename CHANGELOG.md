# Revision history for moe-bangumi

## 1.2.0 -- 2026-02-14

* Add Mikan search API with in-app RSS URL selector
* Enhance tracking modal with RSS search and episode offset configuration
* Allow subscribing bangumi without mikanId
* Improve frontend UI with prettier, tailwindcss plugin, and tracking card badges
* Simplify File.hs and adopt Plex directory naming convention
* Improve special directory handling in collection rename strategy
* Separate binary build from Docker image build in release workflow
* Remove Emby media server integration (Plex-focused)
* Fix subtitle file naming to use Plex-compatible language codes
* Refactor frontend with lazy loading and responsive grid layout

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
