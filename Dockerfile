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
# Purpose: Build dependencies and application with BuildKit cache mounts
# Cache strategy: cabal store and dist-newstyle persist across builds
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
# Layer 1: Cabal configuration (cached unless .cabal or cabal.project changes)
# ============================================================================

# Copy cabal files (cabal.project has source-repository-package deps)
COPY moe-bangumi.cabal cabal.project ./

# Remove test-suite from cabal file to avoid test dependencies
RUN sed -i '/^test-suite/,$d' moe-bangumi.cabal

# Explicitly disable tests and benchmarks for Docker build
RUN echo "tests: False" > cabal.project.local && \
    echo "benchmarks: False" >> cabal.project.local && \
    echo "package moe-bangumi" >> cabal.project.local && \
    echo "  tests: False" >> cabal.project.local && \
    echo "  benchmarks: False" >> cabal.project.local

# ============================================================================
# Layer 2: Build dependencies (cached unless dependencies change)
# ============================================================================

# Build dependencies without source code
# Note: BuildKit cache mount on ~/.cabal/store ensures dependencies are not rebuilt
RUN --mount=type=cache,target=/root/.cabal/store \
    --mount=type=cache,target=/root/.cabal/packages \
    cabal update && \
    cabal build exe:moe-cli --only-dependencies \
        --disable-tests \
        --disable-benchmarks \
        --enable-executable-static \
        --ghc-options='-optl-pthread'

# ============================================================================
# Layer 3: Copy source code (invalidated on any code change)
# ============================================================================

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
# Dependencies are already built and cached, only app code is compiled here
# Note: dist-newstyle is NOT cache-mounted to avoid binary not found issues
RUN --mount=type=cache,target=/root/.cabal/store \
    --mount=type=cache,target=/root/.cabal/packages \
    cabal build exe:moe-cli \
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

# Copy migrations directory (needed at runtime for database setup)
COPY --from=builder /build/migrations /app/migrations

# Create data directory for SQLite database
RUN mkdir -p /app/data

# Expose the default port
EXPOSE 3000

# Set default environment variables
# DATA_FOLDER: where SQLite database and settings are stored
# PORT: HTTP port (default 3000)
# ENV: production or development
ENV DATA_FOLDER=/app/data \
    PORT=3000 \
    ENV=production

ENTRYPOINT ["/app/moe-bangumi"]
