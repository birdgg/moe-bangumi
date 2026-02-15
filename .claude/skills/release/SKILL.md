---
name: release
description: Create a new release version. Validates, tests, builds, and pushes a tagged release. Use when asked to "release", "publish", "bump version", or "create a new version".
disable-model-invocation: true
---

# Release

Create a new release for moe-bangumi.

## Usage

The user provides a version number (e.g., `/release 1.4.0`). If no version is provided, ask the user.

## Workflow

### Step 1: Validate Version

1. Parse the version from the user's input
2. Verify it follows semver format: `X.Y.Z`
3. Check that the tag `vX.Y.Z` does not already exist:
   ```bash
   git tag -l "vX.Y.Z"
   ```
4. Check the current version in `moe-bangumi.cabal` and confirm the bump makes sense

### Step 2: Pre-release Checks

Run these checks and stop if any fail:

```bash
# Ensure working tree is clean
git status --porcelain

# Run tests
cabal test

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
