CREATE TABLE IF NOT EXISTS tracking (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  bangumi_id INTEGER NOT NULL REFERENCES bangumi(id) ON DELETE CASCADE,
  tracking_type TEXT NOT NULL DEFAULT 'subscription',
  rss_url TEXT,
  last_pubdate TEXT,
  current_episode INTEGER NOT NULL DEFAULT 0,
  created_at TEXT NOT NULL DEFAULT (datetime('now')),
  UNIQUE(bangumi_id)
);

CREATE INDEX IF NOT EXISTS idx_tracking_bangumi_id ON tracking(bangumi_id);
