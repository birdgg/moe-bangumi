# ============================================================================
# moe-bangumi Dockerfile
# Multi-stage build for static Haskell binary using ghc-musl
# Supports: linux/amd64, linux/arm64
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

# Copy cabal files (cabal.project has source-repository-package deps)
COPY moe-bangumi.cabal cabal.project ./

# Copy source code (needed for cabal to resolve library modules during dependency build)
COPY app ./app
COPY src ./src
COPY migrations ./migrations
COPY LICENSE CHANGELOG.md ./

# Explicitly disable tests and benchmarks for Docker build
RUN echo "tests: False" > cabal.project.local && \
    echo "benchmarks: False" >> cabal.project.local && \
    echo "package moe-bangumi" >> cabal.project.local && \
    echo "  tests: False" >> cabal.project.local && \
    echo "  benchmarks: False" >> cabal.project.local

# Build dependencies (source code is needed to configure the library)
# Note: BuildKit cache mount on ~/.cabal/store ensures dependencies are not rebuilt
RUN --mount=type=cache,target=/root/.cabal/store \
    --mount=type=cache,target=/root/.cabal/packages \
    cabal update && \
    cabal build exe:moe-cli --only-dependencies \
        --disable-tests \
        --disable-benchmarks \
        --enable-executable-static \
        --ghc-options='-optl-pthread'

# Build the application with static linking
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

# Copy frontend build output (pre-built by CI, downloaded to ./frontend-dist)
COPY frontend-dist /app/web/dist

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
