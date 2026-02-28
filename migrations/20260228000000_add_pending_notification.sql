CREATE TABLE IF NOT EXISTS pending_notification (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  info_hash TEXT NOT NULL,
  title TEXT NOT NULL,
  poster_url TEXT,
  created_at TEXT NOT NULL DEFAULT (datetime('now'))
);
