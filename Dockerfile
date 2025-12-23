# Stage 1: Build frontend
FROM oven/bun:1 AS frontend
WORKDIR /app/web
COPY web/package.json web/bun.lock* ./
RUN bun install --frozen-lockfile
COPY web .
RUN bun run build

# Stage 2: Build Rust binary with musl (static linking)
FROM messense/rust-musl-cross:x86_64-musl AS builder-amd64
WORKDIR /app
RUN cargo install cargo-chef
COPY . .
RUN cargo chef prepare --recipe-path recipe.json
RUN cargo chef cook --release --target x86_64-unknown-linux-musl --recipe-path recipe.json
RUN cargo build --release --target x86_64-unknown-linux-musl -p cli
RUN cp /app/target/x86_64-unknown-linux-musl/release/moe /app/moe

FROM messense/rust-musl-cross:aarch64-musl AS builder-arm64
WORKDIR /app
RUN cargo install cargo-chef
COPY . .
RUN cargo chef prepare --recipe-path recipe.json
RUN cargo chef cook --release --target aarch64-unknown-linux-musl --recipe-path recipe.json
RUN cargo build --release --target aarch64-unknown-linux-musl -p cli
RUN cp /app/target/aarch64-unknown-linux-musl/release/moe /app/moe

# Stage 3: Select the correct binary based on target architecture
ARG TARGETARCH
FROM builder-${TARGETARCH} AS builder

# Stage 4: Runtime image (alpine for smaller size)
FROM alpine:3.21 AS runtime
WORKDIR /app

# Install runtime dependencies
RUN apk add --no-cache ca-certificates curl

# Copy binary from builder
COPY --from=builder /app/moe /app/moe

# Copy frontend dist
COPY --from=frontend /app/web/dist /app/dist

# Create data directory
RUN mkdir -p /data

# Set environment variables
ENV APP_ENV=prod
ENV PORT=3000
ENV DATA_PATH=/data
# TMDB_API_KEY is required and must be provided at runtime:
# docker run -e TMDB_API_KEY=your_key_here ...

EXPOSE 3000

VOLUME ["/data"]

# Healthcheck
HEALTHCHECK --interval=30s --timeout=3s --start-period=5s --retries=3 \
    CMD curl -f http://localhost:3000/docs || exit 1

CMD ["/app/moe"]
