# Stage 1: Build frontend
FROM oven/bun:1 AS frontend
WORKDIR /app/web
COPY web/package.json web/bun.lock* ./
RUN bun install --frozen-lockfile
COPY web .
RUN bun run build

# Stage 2: Compute recipe
FROM rust:1-alpine AS planner
WORKDIR /app
COPY . .
RUN cargo install cargo-chef
RUN cargo chef prepare --recipe-path recipe.json

# Stage 3: Build Rust binary (native Alpine build = musl linked)
FROM rust:1-alpine AS builder
WORKDIR /app
RUN apk add --no-cache musl-dev git
RUN cargo install cargo-chef
COPY --from=planner /app/recipe.json recipe.json
RUN CARGO_NET_GIT_FETCH_WITH_CLI=true cargo chef cook --release --recipe-path recipe.json
COPY . .
RUN cargo build --release -p cli

# Stage 4: Runtime image
FROM alpine:3.21 AS runtime
WORKDIR /app

# Install runtime dependencies and create user
RUN apk add --no-cache ca-certificates curl tzdata \
    && addgroup -S moe && adduser -S moe -G moe

# Copy binary from builder
COPY --from=builder --chown=moe:moe /app/target/release/moe /app/moe

# Copy frontend dist
COPY --from=frontend --chown=moe:moe /app/web/dist /app/dist

# Create data directory and set permissions
RUN mkdir -p /data && chown moe:moe /data

# Set environment variables
ENV APP_ENV=prod
ENV PORT=3000

USER moe

EXPOSE 3000

VOLUME ["/data"]

CMD ["/app/moe"]