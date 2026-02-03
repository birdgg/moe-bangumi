CREATE TABLE IF NOT EXISTS episode (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  bangumi_id INTEGER NOT NULL REFERENCES bangumi(id) ON DELETE CASCADE,
  episode_number INTEGER NOT NULL,
  subtitle_group TEXT,
  resolution TEXT,
  info_hash TEXT NOT NULL,
  torrent_url TEXT NOT NULL,
  pub_date TEXT NOT NULL,
  created_at TEXT NOT NULL DEFAULT (datetime('now')),
  UNIQUE(bangumi_id, episode_number)
);

CREATE INDEX IF NOT EXISTS idx_episode_bangumi_id ON episode(bangumi_id);
