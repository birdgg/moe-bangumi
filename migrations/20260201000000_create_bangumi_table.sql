CREATE TABLE IF NOT EXISTS bangumi (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  title_chs TEXT,
  title_jap TEXT,
  air_date TEXT,
  season_number INTEGER,
  kind TEXT,
  mikan_id INTEGER,
  tmdb_id INTEGER,
  bangumi_tv_id INTEGER UNIQUE,
  poster_url TEXT
);

CREATE INDEX IF NOT EXISTS idx_bangumi_air_date ON bangumi(air_date);
CREATE INDEX IF NOT EXISTS idx_bangumi_bangumi_tv_id ON bangumi(bangumi_tv_id);
