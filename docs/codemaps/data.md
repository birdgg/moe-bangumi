# 数据模型

> 最后更新: 2026-01-30

## 核心领域模型

### Bangumi（番剧）

```haskell
data Bangumi = Bangumi
  { id          :: Maybe BangumiId
  , name        :: Text
  , year        :: Maybe Word16
  , animeSeason :: Maybe AnimeSeason
  , mikanId     :: Maybe MikanId
  , tmdbId      :: Maybe TmdbId
  , bgmtvId     :: Maybe BgmtvId
  , posterUrl   :: Maybe Text
  , overview    :: Maybe Text
  }
```

### ID 类型

| 类型 | 底层 | 用途 |
|------|------|------|
| `BangumiId` | Int64 | 本地数据库 ID |
| `TmdbId` | Word32 | TMDB ID |
| `BgmtvId` | Word32 | Bangumi.tv ID |
| `MikanId` | Word32 | Mikan ID |

### AnimeSeason（动画季度）

```haskell
data AnimeSeason = AnimeSeason
  { year   :: Word16
  , season :: Season  -- Winter | Spring | Summer | Fall
  }
```

### BangumiFile（番剧文件）

```haskell
data BangumiFile = BangumiFile
  { meta     :: BangumiMeta
  , content  :: BangumiContent
  , fileType :: FileType
  }

data BangumiMeta = BangumiMeta
  { name   :: Text
  , year   :: Maybe Year
  , tmdbId :: Maybe TmdbId
  }
```

### BangumiContent（内容类型）

```haskell
data BangumiContent
  = Episode EpisodeType     -- 正片
  | Extra ExtraContent      -- 额外内容 (NCOP/NCED/Menu)
  | TrailerItem TrailerContent  -- 预告片
  | Movie Year              -- 剧场版

data EpisodeType
  = Regular SeasonNum EpisodeNum  -- S01E01
  | Special EpisodeNum            -- SP01
```

### FileType（文件类型）

```haskell
data FileType
  = Video VideoExt                    -- mkv, mp4, avi, webm
  | Subtitle SubtitleLang SubtitleExt -- srt, ass, ssa, sub

data SubtitleLang = CHS | CHT | JPN | ENG
```

## 数据库 Schema

### bangumis

| 字段 | 类型 | 说明 |
|------|------|------|
| id | INTEGER | 主键 |
| title | TEXT | 原始标题 |
| title_cn | TEXT | 中文标题 |
| year | INTEGER | 年份 |
| season | INTEGER | 季度 1-4 |
| air_date | TEXT | 开播时间 (ISO 8601) |
| tmdb_id | TEXT | TMDB ID |
| mikan_id | TEXT | Mikan ID |
| bgmtv_id | TEXT | Bangumi.tv ID |
| poster_url | TEXT | 海报 URL |
| created_at | TEXT | 创建时间 |

### tracking

| 字段 | 类型 | 说明 |
|------|------|------|
| id | INTEGER | 主键 |
| bangumi_id | INTEGER | FK -> bangumis |
| rss_url | TEXT | RSS 订阅 URL |
| last_pubdate | TEXT | 最后处理的 pubDate |
| created_at | TEXT | 创建时间 |

### episodes

| 字段 | 类型 | 说明 |
|------|------|------|
| id | INTEGER | 主键 |
| bangumi_id | INTEGER | FK -> bangumis |
| season | INTEGER | 季数 |
| episode | INTEGER | 集数 |
| source | TEXT | `subscribe` 或 `collection` |
| group_name | TEXT | 字幕组名 |
| torrent_hash | TEXT | qBittorrent hash |
| file_path | TEXT | 最终文件路径 |
| downloaded_at | TEXT | 下载时间 |

## 表关系

```
bangumis (1) --< (N) tracking
    |
    +--< (N) episodes
```

## 外部 API 类型

### BangumiData (bangumi-data)

```haskell
data BangumiDataItem = BangumiDataItem
  { title       :: Text
  , titleCn     :: Maybe Text
  , airDate     :: Text
  , mikanId     :: Maybe MikanId
  , tmdbId      :: Maybe TmdbId
  , bgmtvId     :: Maybe BgmtvId
  }
```

### Metadata 搜索结果

```haskell
-- Bgm.tv 搜索
searchBgmtv :: Text -> Maybe Word16 -> Eff es [Bangumi]

-- TMDB 搜索
searchTmdb :: Text -> Maybe Word16 -> Eff es [Bangumi]
```
