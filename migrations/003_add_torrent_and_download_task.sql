-- Torrent table: stores torrent metadata for bangumi episodes
-- Each torrent is identified by its unique info_hash (globally unique)
CREATE TABLE IF NOT EXISTS torrent (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    created_at DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,

    -- Foreign key to bangumi (required)
    bangumi_id INTEGER NOT NULL REFERENCES bangumi(id) ON DELETE CASCADE,

    -- Optional reference to source RSS (SET NULL when RSS is deleted)
    rss_id INTEGER REFERENCES rss(id) ON DELETE SET NULL,

    -- Torrent identification
    info_hash TEXT NOT NULL,                        -- BitTorrent info hash (40-char hex for v1, 64-char for v2)
    episode_number INTEGER NOT NULL                 -- Episode number this torrent represents
);

-- Torrent indexes
CREATE INDEX IF NOT EXISTS idx_torrent_bangumi_id ON torrent(bangumi_id);
CREATE INDEX IF NOT EXISTS idx_torrent_rss_id ON torrent(rss_id);
CREATE INDEX IF NOT EXISTS idx_torrent_episode_number ON torrent(episode_number);

-- Unique constraint: info_hash is globally unique (one torrent record per unique hash)
CREATE UNIQUE INDEX IF NOT EXISTS idx_torrent_info_hash ON torrent(info_hash);

-- Composite index for common query: find torrents for a specific bangumi episode
CREATE INDEX IF NOT EXISTS idx_torrent_bangumi_episode ON torrent(bangumi_id, episode_number);

-- Trigger to update updated_at on row modification
CREATE TRIGGER IF NOT EXISTS update_torrent_timestamp
AFTER UPDATE ON torrent
FOR EACH ROW
BEGIN
    UPDATE torrent SET updated_at = CURRENT_TIMESTAMP WHERE id = OLD.id;
END;

-- Download task table: tracks download jobs and their status
-- Keeps full history of downloads (completed/failed tasks are retained)
CREATE TABLE IF NOT EXISTS download_task (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    created_at DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,

    -- Foreign key to torrent (required)
    torrent_id INTEGER NOT NULL REFERENCES torrent(id) ON DELETE CASCADE,

    -- Task status: pending, paused, downloading, completed, failed
    status TEXT NOT NULL DEFAULT 'pending' CHECK(status IN ('pending', 'paused', 'downloading', 'completed', 'failed')),

    -- Error message for failed tasks
    error_message TEXT
);

-- Download task indexes
CREATE INDEX IF NOT EXISTS idx_download_task_torrent_id ON download_task(torrent_id);
CREATE INDEX IF NOT EXISTS idx_download_task_status ON download_task(status);
CREATE INDEX IF NOT EXISTS idx_download_task_created_at ON download_task(created_at);

-- Composite index for common query: find tasks by status ordered by creation time
CREATE INDEX IF NOT EXISTS idx_download_task_status_created ON download_task(status, created_at);

-- Trigger to update updated_at on row modification
CREATE TRIGGER IF NOT EXISTS update_download_task_timestamp
AFTER UPDATE ON download_task
FOR EACH ROW
BEGIN
    UPDATE download_task SET updated_at = CURRENT_TIMESTAMP WHERE id = OLD.id;
END;
