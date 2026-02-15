# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

moe-bangumi is a Bangumi (anime) management tool written in Haskell. It handles file naming and organization for media servers (Plex, Emby, Jellyfin). This is a single-user application.It deployed on NAS.

## Build Commands

```bash
cabal build              # Build the project
cabal run moe-cli        # Run the executable
cabal test               # Run all tests
cabal repl               # Start GHCi with project loaded
```

### Justfile (recommended for development)

```bash
just dev                 # Run backend + frontend in tmux split panes
just dev-backend         # Run backend only (cabal run moe-cli)
just dev-frontend        # Run frontend dev server (bun run dev)
just build               # Build backend + frontend
just gen-api             # Generate API client from OpenAPI spec
just release X.Y.Z       # Bump version, generate changelog, tag, and push
```

## Code Style
- add simple haddoc
- when refer to `anime` use the word `bangumi`
- No suffix for field names (use `name` not `nameField`)
- Use `Display` instance for logging
- Do not add statistical/counting logs (e.g., "Found N items", "Processed N records")
- Regex patterns (`Pattern` from `Moe.Domain.Parser.Internal.Pattern`) should be defined as top-level constants via `mkPattern`; for dynamic patterns (e.g., user config), build them once before iteration (see `filterItems` in `Subscription.hs`)

## Database Migrations

- After adding or modifying a migration SQL file, review ALL queries in `src/Moe/Infra/Database/` to ensure column lists match the updated schema
- Pay special attention to JOIN queries that hardcode column names (e.g., in `Tracking.hs`) — they must include any new columns in the correct position
- Use `bangumiColumnsAs` for bangumi columns in JOIN queries to avoid column drift
- After adding a new migration file, touch `src/Moe/Infra/Database/Embedded.hs` to trigger recompilation (GHC doesn't track directory listings for TH)

## Testing

- Keep tests minimal - test only essential behavior
- Do not write tests for new features unless explicitly requested
- Focus on pure domain logic tests, avoid integration tests for simple features

## Technical Stack

- **Backend**: Haskell with Servant (web framework) + effectful (effect system)
- **Database**: SQLite via sqlite-simple + resource-pool
- **Regex**: PCRE2 (not regex-tdfa)
- GHC 9.14.1 with GHC2024 language standard
- Executable name: `moe-cli` (not `moe-bangumi`)
- Uses `relude` as alternative prelude via `Moe.Prelude`
  - `show` is polymorphic in relude, can return `Text` directly
  - `toText` from `Relude` for type conversions
  - `pass` is available from relude
- Test framework: Tasty with HUnit

### Environment Variables

- `ENV` - `production` or `development` (default: development)
- `PORT` - HTTP port (default: 3000)
- `DATA_FOLDER` - Path for SQLite database and settings (default: `./data`)

## Architecture

- `app/` - Executable entry point (`Main.hs`, `Supervisor.hs`)
- `src/Moe/` - Library code
  - `App/` - Bootstrap, config, logging, monad (`MoeM`)
  - `Domain/` - Pure domain logic (bangumi, episode, parser, tracking, rss)
  - `Infra/` - External adapters (database, downloader, metadata, notification, rss, settings)
  - `Web/` - Servant API server, routes, DTO, embedded SPA
  - `Job/` - Background jobs (subscription, rename, calendar sync)
- `test/` - Tests using Tasty framework
- `migrations/` - SQLite migration SQL files
- `web/` - React frontend (SPA embedded in binary via file-embed)
- `docs/` - Domain documentation (naming conventions for media servers)

### External Services

- **TMDB** - Metadata lookup (poster, air date)
- **BGM.tv / Mikan** - Bangumi search and RSS source
- **qBittorrent** - Torrent download via WebAPI
- **API docs** - Available at `/docs` (ScalarUI + OpenAPI)

## Frontend

- **React 19** + **TypeScript 5.9** + **Vite 7**
- **TanStack Router** for file-based routing (`web/src/routes/`)
- **TanStack Query** for server state management
- **Tailwind CSS v4** + **shadcn** components
- Uses **bun** as package manager and runtime (`bun install`, `bun run dev`, `bun run gen:api`)
- API client is auto-generated via `@hey-api/openapi-ts` from the backend OpenAPI spec

### TanStack Form

- `form` is passed as a prop (`SettingsFormInstance`), not created in the component
- Reading `form.state.values` directly does NOT trigger re-renders
- Use `form.Subscribe` with `selector` to reactively read form values outside of `form.Field`
- `form.Field` already handles subscriptions for its own field value

## Release

- Use `just release X.Y.Z` (or `/release` skill) - handles version bump, changelog, tag, and push
- Follows semver: major (breaking), minor (feature), patch (fix)
- Pushing a `vX.Y.Z` tag triggers CI to build Linux binary and create GitHub Release
- Frontend assets are embedded in the binary via `web/dist/` (file-embed)