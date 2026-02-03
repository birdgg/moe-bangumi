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

- when refer to `anime` use the word `bangumi`
- Do not add comments to code
- No suffix for field names (use `name` not `nameField`)
- Moe module (domain module) must remain pure - no side effects
- After editing Haskell files, run `hlint` on changed files and apply suggestions
- Use `Display` instance when need logging
- Do not add statistical/counting logs (e.g., "Found N items", "Processed N records")

## Learn Pattern
- learn haskell code pattern from learn/src, it is flora server source code

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
