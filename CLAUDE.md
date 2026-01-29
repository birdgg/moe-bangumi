# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

moe-bangumi is a Bangumi (anime) management tool written in Haskell. It handles file naming and organization for media servers (Plex, Emby, Jellyfin).

## Build Commands

```bash
cabal build              # Build the project
cabal run moe-bangumi    # Run the executable
cabal test               # Run all tests
cabal repl               # Start GHCi with project loaded
```

## Code Style

- Do not add comments to code
- No suffix for field names (use `name` not `nameField`)
- Moe module (domain module) must remain pure - no side effects
- After editing Haskell files, run `hlint` on changed files and apply suggestions

## Technical Stack

- GHC 9.12.2 with GHC2024 language standard
- Uses Relude as alternative Prelude
  - `show` returns `Text` (not `String`), use directly without `T.pack`
  - `toString` converts `Text` to `String`/`FilePath`, use instead of `T.unpack`
- Test framework: Tasty with HUnit

## Architecture

- `src/` - Library code
  - `Bangumi/Internal/` - Internal modules not exported publicly
- `app/` - Executable entry point
- `test/` - Tests using Tasty framework
- `docs/` - Domain documentation (naming conventions for media servers)
