# Changelog
## [1.5.0] - 2026-02-24

### Bug Fixes

- Use dataFolder for update temp directory to fix Docker permission error

### Features

- Add cleanup job to auto-delete seeded torrents

## [1.4.0] - 2026-02-24

### Features

- Refactor BangumiContent and improve import scan

## [1.3.5] - 2026-02-15

### Bug Fixes

- Use BusyBox-compatible tar flags for auto-update extraction

## [1.3.4] - 2026-02-15

### Bug Fixes

- Add missing first_air_year in JOIN queries and refactor shared column list

## [1.3.3] - 2026-02-15

### Features

- Add development commands to justfile
- Add firstAirYear field and embed migrations at compile time

## [1.3.2] - 2026-02-14

### Bug Fixes

- Optimize Docker build to avoid duplicate frontend builds

## [1.3.1] - 2026-02-14

### Bug Fixes

- Remove unused queryClient in SystemSection
- Remove unused queryClient and bump version to 1.3.1

## [1.3.0] - 2026-02-14

### Bug Fixes

- Close TOCTOU race condition in supervisor shutdown

### Features

- Implement self-update functionality with embedded frontend
- Implement safe binary replacement with backup and rollback

## [1.2.0] - 2026-02-14

### Bug Fixes

- Use Plex-compatible language codes for subtitle file naming
- Optimize Docker build cache strategy
- Create placeholder directories for cabal dependency build
- Copy source code before dependency build
- Remove test-suite from cabal file in Docker build
- Resolve GHC compilation warnings

### Features

- Allow subscribing bangumi without mikanId via TrackingModal
- Enhance tracking modal with RSS search, episode offset API, and UX improvements
- Add Mikan search API and in-app selector for RSS URL
- Add prettier with tailwindcss plugin and improve tracking card badge
- Add GitHub link to header

## [1.1.0] - 2026-02-13

### Bug Fixes

- Handle TestConnection independently of saved downloader config
- Add SPA fallback for frontend routes
- Improve RSS error formatting and error handling

### Features

- Import Emby watch progress and auto-subscribe current season RSS

## [1.0.0] - 2026-02-13

### Bug Fixes

- Update bun lockfile
- Resolve TypeScript build errors in frontend
- Copy cabal.project in Dockerfile for source-repository deps
- Only build exe dependencies in Dockerfile
- Disable tests and benchmarks in Dockerfile dependency build
- Use --disable-tests instead of exe target in Dockerfile
- Use correct executable name moe-cli in Dockerfile
- Move tests/benchmarks config to cabal.project for consistent Docker stages
- Remove -optl-static to fix GHC 9.14.1 static linking
- Simplify error Display instances to reduce redundant colons
- Gracefully handle unconfigured TMDB and downloader
- Optimize Dockerfile with BuildKit cache mounts and add docker-run
- Switch Docker build cache from GHA to registry-based

### Features

- Serve frontend static files from web/dist via Servant Raw endpoint

