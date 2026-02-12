CREATE TABLE IF NOT EXISTS bangumi (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  title_chs TEXT,
  title_jap TEXT,
  air_date TEXT NOT NULL,
  season INTEGER,
  kind TEXT,
  mikan_id INTEGER,
  tmdb_id INTEGER,
  bgmtv_id INTEGER UNIQUE,
  poster_url TEXT,
  total_episodes INTEGER,
  created_at TEXT NOT NULL DEFAULT (datetime('now')),
  updated_at TEXT NOT NULL DEFAULT (datetime('now'))
);

CREATE INDEX IF NOT EXISTS idx_bangumi_air_date ON bangumi(air_date);
CREATE INDEX IF NOT EXISTS idx_bangumi_bgmtv_id ON bangumi(bgmtv_id);

CREATE TABLE IF NOT EXISTS tracking (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  bangumi_id INTEGER NOT NULL REFERENCES bangumi(id) ON DELETE CASCADE,
  tracking_type TEXT NOT NULL DEFAULT 'subscription',
  rss_url TEXT,
  rss_enabled INTEGER NOT NULL DEFAULT 1,
  last_pubdate TEXT,
  current_episode INTEGER NOT NULL DEFAULT 0,
  episode_offset INTEGER NOT NULL DEFAULT 0,
  is_bdrip INTEGER NOT NULL DEFAULT 0,
  auto_complete INTEGER NOT NULL DEFAULT 1,
  created_at TEXT NOT NULL DEFAULT (datetime('now')),
  updated_at TEXT NOT NULL DEFAULT (datetime('now')),
  UNIQUE(bangumi_id)
);

CREATE INDEX IF NOT EXISTS idx_tracking_bangumi_id ON tracking(bangumi_id);

CREATE TRIGGER tracking_disable_rss_on_collection_insert
AFTER INSERT ON tracking
WHEN NEW.tracking_type = 'collection' AND NEW.rss_enabled = 1
BEGIN
  UPDATE tracking SET rss_enabled = 0 WHERE id = NEW.id;
END;

CREATE TRIGGER tracking_disable_rss_on_collection_update
AFTER UPDATE OF tracking_type ON tracking
WHEN NEW.tracking_type = 'collection' AND NEW.rss_enabled = 1
BEGIN
  UPDATE tracking SET rss_enabled = 0 WHERE id = NEW.id;
END;

CREATE TABLE IF NOT EXISTS episode (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  bangumi_id INTEGER NOT NULL REFERENCES bangumi(id) ON DELETE CASCADE,
  episode_number INTEGER NOT NULL,
  "group" TEXT,
  subtitle_list TEXT,
  resolution TEXT,
  info_hash TEXT NOT NULL,
  torrent_url TEXT NOT NULL,
  pub_date TEXT NOT NULL,
  created_at TEXT NOT NULL DEFAULT (datetime('now')),
  updated_at TEXT NOT NULL DEFAULT (datetime('now')),
  UNIQUE(bangumi_id, episode_number)
);

CREATE INDEX IF NOT EXISTS idx_episode_bangumi_id ON episode(bangumi_id);
