# 数据库设计

## Schema

```sql
CREATE TABLE bangumis (
  id INTEGER PRIMARY KEY,
  title TEXT NOT NULL,
  title_cn TEXT,
  year INTEGER,
  season INTEGER,
  air_date TEXT,
  tmdb_id TEXT,
  mikan_id TEXT,
  bgmtv_id TEXT,
  poster_url TEXT,
  created_at TEXT NOT NULL DEFAULT (datetime('now'))
);

CREATE TABLE tracking (
  id INTEGER PRIMARY KEY,
  bangumi_id INTEGER NOT NULL REFERENCES bangumis(id),
  rss_url TEXT NOT NULL,
  last_pubdate TEXT,
  created_at TEXT NOT NULL DEFAULT (datetime('now')),
  UNIQUE(bangumi_id)
);

CREATE TABLE episodes (
  id INTEGER PRIMARY KEY,
  bangumi_id INTEGER NOT NULL REFERENCES bangumis(id),
  season INTEGER NOT NULL,
  episode INTEGER NOT NULL,
  source TEXT NOT NULL,
  group_name TEXT,
  torrent_hash TEXT,
  file_path TEXT,
  downloaded_at TEXT NOT NULL DEFAULT (datetime('now')),
  UNIQUE(bangumi_id, season, episode)
);

CREATE INDEX idx_bangumis_mikan ON bangumis(mikan_id);
CREATE INDEX idx_bangumis_year_season ON bangumis(year, season);
CREATE INDEX idx_episodes_bangumi ON episodes(bangumi_id);
```

## 表说明

### bangumis

番剧基本信息，数据来源于 bangumi-data。

| 字段 | 类型 | 说明 |
|------|------|------|
| id | INTEGER | 主键 |
| title | TEXT | 原始标题（日文） |
| title_cn | TEXT | 中文标题 |
| year | INTEGER | 年份 |
| season | INTEGER | 季度 1-4 |
| air_date | TEXT | 开播时间，ISO 8601 格式 |
| tmdb_id | TEXT | TMDB ID，格式如 `tv/209867` |
| mikan_id | TEXT | Mikan ID，用于生成 RSS URL |
| bgmtv_id | TEXT | Bangumi.tv ID |
| poster_url | TEXT | 海报图片 URL |
| created_at | TEXT | 创建时间 |

### tracking

追番记录。

| 字段 | 类型 | 说明 |
|------|------|------|
| id | INTEGER | 主键 |
| bangumi_id | INTEGER | 关联番剧 |
| rss_url | TEXT | RSS 订阅 URL |
| last_pubdate | TEXT | 最后处理的 RSS pubDate，用于过滤已处理项 |
| created_at | TEXT | 创建时间 |

### episodes

剧集下载记录，用于洗版比较。每集只保留一条记录，洗版时更新。

| 字段 | 类型 | 说明 |
|------|------|------|
| id | INTEGER | 主键 |
| bangumi_id | INTEGER | 关联番剧 |
| season | INTEGER | 季数 |
| episode | INTEGER | 集数 |
| source | TEXT | 来源：`subscribe` 或 `collection` |
| group_name | TEXT | 当前版本的字幕组 |
| torrent_hash | TEXT | qBittorrent torrent hash |
| file_path | TEXT | 最终文件路径 |
| downloaded_at | TEXT | 下载时间 |

覆盖规则：`collection` 可覆盖 `subscribe`，反之不行。

## 表关系

```
bangumis (1) ──< (N) tracking
    │
    └──< (N) episodes
```

## 常用查询

### 用户库

通过 episodes 关联 bangumis 获取用户所有番剧：

```sql
SELECT b.* FROM bangumis b
WHERE EXISTS (SELECT 1 FROM episodes e WHERE e.bangumi_id = b.id)
```
