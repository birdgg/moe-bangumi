# Bump cabal version, commit, tag, and push to trigger CI
# Usage: just release 0.5.1
release version:
    #!/usr/bin/env bash
    set -euo pipefail
    tag="v{{version}}"

    if ! [[ "{{version}}" =~ ^[0-9]+\.[0-9]+\.[0-9]+(-[a-zA-Z0-9.]+)?$ ]]; then
        echo "Invalid version format: {{version}}"
        echo "Expected: X.Y.Z or X.Y.Z-suffix"
        exit 1
    fi

    if git rev-parse "$tag" >/dev/null 2>&1; then
        echo "Tag $tag already exists"
        exit 1
    fi

    # Bump version in cabal file
    sed -i '' "s/^version: .*/version: {{version}}/" moe-bangumi.cabal

    # Generate changelog with git-cliff
    git tag "$tag"
    git-cliff -o CHANGELOG.md
    git tag -d "$tag"

    git add moe-bangumi.cabal CHANGELOG.md
    git commit -m "chore: release {{version}}"
    git tag "$tag"
    git push origin main "$tag"
    echo "Pushed $tag — CI will build Docker image and create GitHub Release"

# Run backend and frontend in tmux split panes
dev:
    #!/usr/bin/env bash
    set -euo pipefail
    root="$(pwd)"
    session="moe-dev"
    tmux kill-session -t "$session" 2>/dev/null || true
    tmux new-session -d -s "$session" -n dev -c "$root"
    tmux send-keys -t "$session" 'just dev-backend' Enter
    tmux split-window -h -t "$session" -c "$root"
    tmux send-keys -t "$session" 'just dev-frontend' Enter
    tmux attach -t "$session"

# Run backend only
dev-backend *RTS_OPTS:
    cabal run moe-cli -- +RTS -s {{RTS_OPTS}} -RTS

# Run frontend dev server
dev-frontend:
    cd web && bun run dev

# Build everything
build:
    cabal build
    cd web && bun run build

# Generate API client from OpenAPI spec
gen-api:
    cd web && bun run gen:api

# Build and run local Docker image
docker-run:
    docker build --load -t moe-bangumi .
    docker run --rm -p 3000:3000 -v moe-bangumi-data:/app/data moe-bangumi
