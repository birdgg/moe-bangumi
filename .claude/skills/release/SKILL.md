---
name: release
description: Create a new release version. Validates, tests, builds, and pushes a tagged release. Use when asked to "release", "publish", "bump version", or "create a new version".
---

# Release

Create a new release for moe-bangumi.

## Usage

`/release` — auto-detect version from conventional commits.
`/release 1.8.0` — use the specified version directly (skip auto-detection).

## Workflow

### Step 1: Determine Version

#### If version is provided as argument:
1. Parse the version from the user's input
2. Verify it follows semver format: `X.Y.Z`
3. Check that the tag `vX.Y.Z` does not already exist:
   ```bash
   git tag -l "vX.Y.Z"
   ```

#### If no version is provided (auto-detect):
1. Get the current version from `moe-bangumi.cabal`:
   ```bash
   grep '^version:' moe-bangumi.cabal | awk '{print $2}'
   ```
2. Get commits since the last tag:
   ```bash
   git log $(git describe --tags --abbrev=0)..HEAD --oneline
   ```
3. If there are no `feat:` or `fix:` commits, warn the user and stop.
4. Determine the bump type:
   - If any commit message contains `BREAKING CHANGE` in the body or uses `!:` (e.g., `feat!:`) → **major** bump (X+1.0.0)
   - If any commit starts with `feat` → **minor** bump (X.Y+1.0)
   - If only `fix` commits → **patch** bump (X.Y.Z+1)
5. Calculate the next version and present it to the user along with the commit summary.
6. Proceed with the detected version (no need to ask for confirmation).

### Step 2: Pre-release Checks

Run these checks and stop if any fail:

```bash
# Ensure working tree is clean
git status --porcelain

# Build frontend
cd web && bun run build
```

### Step 3: Generate Changelog

Review commits since the last tag to understand what changed:

```bash
git log $(git describe --tags --abbrev=0)..HEAD --oneline
```

Present a summary of changes to the user for confirmation.

### Step 4: Execute Release

Use the justfile release command which handles everything atomically:

```bash
just release X.Y.Z
```

This command:
1. Validates version format
2. Checks tag doesn't exist
3. Updates version in `moe-bangumi.cabal`
4. Generates CHANGELOG.md via git-cliff
5. Commits with message `chore: release X.Y.Z`
6. Creates git tag `vX.Y.Z`
7. Pushes to origin (main branch + tag)

### Step 5: Verify

After pushing, the GitHub Action (`.github/workflows/release.yml`) will automatically:
- Build the Linux binary (static, via ghc-musl)
- Create a GitHub Release with the binary attached

Check the action status:
```bash
gh run list --limit 1
```

Report the release URL to the user.
