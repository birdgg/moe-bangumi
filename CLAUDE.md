# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

moe-bangumi is a Bangumi (anime) management tool written in Haskell. It handles file naming and organization for media servers (Plex, Emby, Jellyfin). This is a single-user application.It deployed on NAS.

## Build Commands

```bash
cabal build              # Build the project
cabal run moe-bangumi    # Run the executable
cabal test               # Run all tests
cabal repl               # Start GHCi with project loaded
```

## Code Style
- add simple haddoc
- when refer to `anime` use the word `bangumi`
- No suffix for field names (use `name` not `nameField`)
- After editing Haskell files, run `hlint` on changed files and apply suggestions
- Use `Display` instance for logging
- Do not add statistical/counting logs (e.g., "Found N items", "Processed N records")
- Regex patterns (`Pattern` from `Moe.Domain.Parser.Internal.Pattern`) should be defined as top-level constants via `mkPattern`; for dynamic patterns (e.g., user config), build them once before iteration (see `filterItems` in `Subscription.hs`)

## Database Migrations

- After adding or modifying a migration SQL file, review ALL queries in `src/Moe/Infra/Database/` to ensure column lists match the updated schema
- Pay special attention to JOIN queries that hardcode column names (e.g., in `Tracking.hs`) — they must include any new columns in the correct position
- Use `bangumiColumnsAs` for bangumi columns in JOIN queries to avoid column drift

## Testing

- Keep tests minimal - test only essential behavior
- Do not write tests for new features unless explicitly requested
- Focus on pure domain logic tests, avoid integration tests for simple features

## Technical Stack

- GHC 9.14.1 with GHC2024 language standard
- Uses `relude` as alternative prelude via `Moe.Prelude`
  - `show` is polymorphic in relude, can return `Text` directly
  - `toText` from `Relude` for type conversions
  - `pass` is available from relude
- Test framework: Tasty with HUnit

## Architecture

- `src/` - Library code
  - `Bangumi/Internal/` - Internal modules not exported publicly
- `app/` - Executable entry point
- `test/` - Tests using Tasty framework
- `docs/` - Domain documentation (naming conventions for media servers)

## Effect Pattern

- Use `effectful-th` to generate send functions: `makeEffect ''EffectName`
- Effect definitions go in `src/Moe/Effect/`
- Effect interpreters (implementations) go in `src/Moe/Adapter/`

## Frontend

- Uses **bun** as package manager and runtime (`bun install`, `bun run dev`, `bun run gen:api`)
- API client is auto-generated via `@hey-api/openapi-ts` from the backend OpenAPI spec

### TanStack Form

- `form` is passed as a prop (`SettingsFormInstance`), not created in the component
- Reading `form.state.values` directly does NOT trigger re-renders
- Use `form.Subscribe` with `selector` to reactively read form values outside of `form.Field`
- `form.Field` already handles subscriptions for its own field value

## Release Workflow

### Version Bump Process

1. **Update version in `moe-bangumi.cabal`** (CRITICAL - must be done before tagging)
   ```cabal
   version: X.Y.Z
   ```

2. **Commit version bump**
   ```bash
   git add moe-bangumi.cabal
   git commit -m "chore: bump version to X.Y.Z"
   git push
   ```

3. **Create and push git tag**
   ```bash
   git tag -a vX.Y.Z -m "Release vX.Y.Z

   New Features:
   - Feature 1
   - Feature 2

   Bug Fixes:
   - Fix 1
   "
   git push origin vX.Y.Z
   ```

### Versioning Scheme

Follow semantic versioning (semver):
- **Major (X.0.0)**: Breaking changes
- **Minor (X.Y.0)**: New features, backward compatible
- **Patch (X.Y.Z)**: Bug fixes only

### GitHub Actions

Pushing a tag (`vX.Y.Z`) automatically triggers:
- `.github/workflows/release.yml` - Builds Linux binary and creates GitHub release
- Frontend assets are embedded in the binary via `web/dist/`

### Pre-release Checklist

- [ ] Version number updated in `moe-bangumi.cabal`
- [ ] All tests passing locally (`cabal test`)
- [ ] Frontend builds successfully (`cd web && bun run build`)
- [ ] All changes committed and pushed to main
- [ ] Tag message includes clear changelog