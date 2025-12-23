-- Generic cache table for external API responses
CREATE TABLE IF NOT EXISTS cache (
    cache_key TEXT PRIMARY KEY,
    data TEXT NOT NULL,
    fetched_at INTEGER NOT NULL  -- Unix timestamp
);

-- Index for cleanup queries
CREATE INDEX IF NOT EXISTS idx_cache_fetched_at ON cache(fetched_at);
