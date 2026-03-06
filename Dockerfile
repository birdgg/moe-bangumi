# ============================================================================
# moe-bangumi Dockerfile
# Multi-stage build for static Haskell binary using ghc-musl
# Supports: linux/amd64, linux/arm64
#
# NOTE: Requires web/dist to exist before building
# In GitHub Actions: frontend is built separately in build-frontend job
# For local builds: run `cd web && bun run build` first
# ============================================================================

# ============================================================================
# Stage 1: Builder
# Purpose: Build dependencies and application
# Cache strategy: registry layer cache via docker/build-push-action
# ============================================================================
FROM quay.io/benz0li/ghc-musl:9.14.1 AS builder

WORKDIR /build

# Install system dependencies for static linking
# - zlib-static: compression (tar, zlib packages)
# - sqlite-static: database (sqlite-simple)
# - pcre2-dev: regex (pcre2 package)
RUN apk add --no-cache \
    zlib-dev \
    zlib-static \
    sqlite-dev \
    sqlite-static \
    pcre2-dev

# ============================================================================
# Layer 1: Cabal configuration (cached unless dependencies change)
# ============================================================================

# Copy cabal files (cabal.project has source-repository-package deps)
COPY moe-bangumi.cabal cabal.project ./

# Normalize version to prevent cache invalidation on version bumps.
# Remove test-suite to avoid test dependencies.
# This ensures the dependency layer is only rebuilt when actual deps change.
RUN sed -i -e 's/^version:.*/version: 0.0.0/' \
           -e '/^test-suite/,$d' moe-bangumi.cabal

# Explicitly disable tests and benchmarks for Docker build
RUN echo "tests: False" > cabal.project.local && \
    echo "benchmarks: False" >> cabal.project.local && \
    echo "package moe-bangumi" >> cabal.project.local && \
    echo "  tests: False" >> cabal.project.local && \
    echo "  benchmarks: False" >> cabal.project.local

# ============================================================================
# Layer 2: Build dependencies (cached unless .cabal or cabal.project changes)
# ============================================================================

# Dependencies are baked into this layer so registry cache can reuse them.
# This layer is only rebuilt when .cabal or cabal.project changes.
RUN cabal update && \
    cabal build --only-dependencies \
        --disable-tests \
        --disable-benchmarks \
        --enable-executable-static \
        --ghc-options='-optl-pthread'

# ============================================================================
# Layer 3: Copy source code (invalidated on any code change)
# ============================================================================

# Re-copy the real cabal file with correct version for the application build
COPY moe-bangumi.cabal ./
RUN sed -i '/^test-suite/,$d' moe-bangumi.cabal

# Copy application source code
COPY app ./app
COPY src ./src
COPY migrations ./migrations
COPY LICENSE CHANGELOG.md ./

# Copy frontend build output (needed at compile time for file-embed TH)
# This must exist before docker build (built by GitHub Actions or locally via `bun run build`)
COPY web/dist ./web/dist

# ============================================================================
# Layer 4: Build application (only rebuilds when source code changes)
# ============================================================================

# Build the application with static linking
# Dependencies are already in the layer from step above, only app code is compiled here
RUN cabal build exe:moe-cli \
        --disable-tests \
        --disable-benchmarks \
        --enable-executable-static \
        --ghc-options='-optl-pthread -split-sections' && \
    # Find and copy the binary to a known location
    find dist-newstyle -type f -name moe-cli -executable \
        -exec cp {} /build/moe-bangumi \; && \
    # Strip debug symbols to reduce binary size
    strip /build/moe-bangumi

# Verify static linking
RUN file /build/moe-bangumi | tee /dev/stderr | grep -q "statically linked" || \
    (echo "ERROR: Binary is not statically linked!" && exit 1)

# ============================================================================
# Stage 2: Runtime Image
# Purpose: Minimal production image
# Size: ~10-20 MB (Alpine base + static binary)
# ============================================================================
FROM alpine:3.21 AS runtime

# Install minimal runtime dependencies
# - ca-certificates: for HTTPS requests (BGM.tv, TMDB, qBittorrent APIs)
# - tzdata: timezone support for scheduling
RUN apk add --no-cache \
    ca-certificates \
    tzdata

WORKDIR /app

# Copy the static binary from builder
COPY --from=builder /build/moe-bangumi /app/moe-bangumi

# Create data directory for SQLite database
# Make /app writable so self-update can replace the binary with any UID
RUN mkdir -p /app/data && chmod a+w /app

# Expose the default port
EXPOSE 3000

# Set default environment variables
# DATA_FOLDER: where SQLite database and settings are stored
# PORT: HTTP port (default 3000)
# ENV: production or development
ENV DATA_FOLDER=/app/data \
    PORT=3000 \
    ENV=production

ENTRYPOINT ["/app/moe-bangumi", "--supervised"]
