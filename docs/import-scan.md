# Import Scan

扫描下载器 savePath 中已有的番剧目录，自动识别元数据并导入 Bangumi + Tracking 记录。

## 触发方式

`POST /api/import/scan` → `importExistingBangumi`

手动触发，扫描结果通过 API 返回给前端展示。

## 整体流程

两阶段设计：Phase 1 做 IO 密集的文件扫描和 TMDB 元数据解析，Phase 2 在单次事务中批量持久化。

```mermaid
flowchart TD
    A[读取 savePath] --> B{目录存在?}
    B -->|否| EMPTY[返回空结果]
    B -->|是| C[listDirectory]
    C --> D["filter isBangumiDir<br/>(排除 . 和 @ 开头)"]

    D --> E["Phase 1: foldM resolveDirectory"]
    E --> F["Phase 2: transact foldM persistResolved"]
    F --> G["ImportResult { imported, skipped }"]
```

## Phase 1: 文件扫描 + TMDB 解析

对每个番剧目录，扫描文件结构并通过 TMDB 获取元数据。使用 `BangumiCache` 避免同一标题重复查询 TMDB。

```mermaid
flowchart TD
    DIR[番剧目录] --> SCAN[scanBangumiDir]

    SCAN --> PFN["parseFolderName<br/>提取 title, year, tmdbId"]
    PFN --> ENTRIES[遍历目录内容]

    ENTRIES --> IS_DIR{是目录?}
    IS_DIR -->|是| PARSE_SEASON{"parseSeasonDir<br/>Season 01 → SeasonIndex 1"}
    PARSE_SEASON -->|匹配| VIDEOS[listVideoFiles → 收入 seasonMap]
    PARSE_SEASON -->|不匹配| SKIP_DIR[忽略]
    IS_DIR -->|否| IS_VIDEO{isVideoExt?}
    IS_VIDEO -->|是| LOOSE[收入 looseFiles]
    IS_VIDEO -->|否| SKIP_FILE[忽略]

    VIDEOS --> MERGE["合并: looseFiles → Season 1"]
    LOOSE --> MERGE
    MERGE --> SB["每个 season 生成 ScannedBangumi"]

    SB --> RESOLVE[resolveMetadata]
    RESOLVE --> CACHE{BangumiCache 命中?}
    CACHE -->|命中| USE_CACHE[使用缓存]
    CACHE -->|未命中| HAS_TMDB{有 TmdbId?}
    HAS_TMDB -->|有| DIRECT["getTmdbTvDetail<br/>失败则 getTmdbMovieDetail"]
    HAS_TMDB -->|没有| SEARCH["searchTmdb(title)"]
    DIRECT --> RESULT["Maybe Bangumi"]
    SEARCH --> RESULT

    RESULT --> APPLY["applySeason<br/>(Movie 不设 season)"]
    APPLY --> RESOLVED["DirReady / DirSkipped"]
```

### parseFolderName 解析规则

支持 Plex/Jellyfin 风格的目录命名：

| 输入 | title | year | tmdbId |
|------|-------|------|--------|
| `葬送的芙莉莲 (2023)` | 葬送的芙莉莲 | 2023 | — |
| `葬送的芙莉莲 (2023) {tmdb-120089}` | 葬送的芙莉莲 | 2023 | 120089 |
| `葬送的芙莉莲` | 葬送的芙莉莲 | — | — |

### 目录结构识别

```
savePath/
├── 葬送的芙莉莲 (2023)/          ← parseFolderName 解析
│   ├── Season 01/                ← parseSeasonDir → SeasonIndex 1
│   │   ├── Frieren - S01E01.mkv  ← video file
│   │   └── Frieren - S01E02.mkv
│   ├── Season 02/                ← SeasonIndex 2
│   │   └── ...
│   └── movie.mkv                 ← loose file → Season 1
├── .hidden/                      ← isBangumiDir 过滤掉
└── @recycle/                     ← isBangumiDir 过滤掉
```

## Phase 2: 事务持久化

所有扫描结果在单次 SQLite 事务中处理。

```mermaid
flowchart TD
    R[ResolvedDir] --> TYPE{类型?}
    TYPE -->|DirSkipped| ADD_SKIP[加入 skipped 列表]
    TYPE -->|DirReady| FIND["BangumiDB.findExistingBangumi"]

    FIND --> EXISTS{已存在?}
    EXISTS -->|存在| HAS_TRACK{"TrackingDB.getTrackingByBangumi"}
    HAS_TRACK -->|有 Tracking| SKIP_TRACKED["跳过 (AlreadyTracked)"]
    HAS_TRACK -->|无 Tracking| CREATE[createBangumiAndTracking]
    EXISTS -->|不存在| CREATE

    CREATE --> UPSERT_B["BangumiDB.upsertBangumi → BangumiId"]
    UPSERT_B --> UPSERT_T["TrackingDB.upsertTracking"]
    UPSERT_T --> ENTRY["ImportedEntry"]
```

### Tracking 记录默认值

导入时创建的 Tracking 记录：

| 字段 | 值 | 说明 |
|------|-----|------|
| `trackingType` | `Collection` | 非 RSS 订阅 |
| `rssEnabled` | `False` | SQLite 触发器强制 |
| `currentEpisode` | `0` | — |
| `episodeOffset` | `0` | — |
| `isBDrip` | `False` | — |
| `autoComplete` | `True` | — |
| `rssUrl` | `Nothing` | — |

### Bangumi 去重策略

`upsertBangumi` 按以下优先级去重：

1. 若有 `bgmtvId` → `ON CONFLICT (bgmtv_id) DO UPDATE`
2. 否则 → `findExistingBangumi`：
   - 先按 `tmdb_id + air_date` 匹配
   - 再按 `title_chs + air_date` 匹配
   - 均未匹配 → 创建新记录

## API 响应

```json
{
  "imported": [
    { "bangumiId": 42, "title": "葬送的芙莉莲", "posterUrl": "https://..." }
  ],
  "skipped": [
    { "folderName": "@recycle", "reason": "no video files found" },
    { "folderName": "已订阅的番", "reason": "already tracked" },
    { "folderName": "未知番剧", "reason": "TMDB search failed" }
  ]
}
```

跳过原因：

| SkipReason | 说明 |
|------------|------|
| `AlreadyTracked` | 该 Bangumi 已有 Tracking 记录 |
| `NoVideoFiles` | 目录内无视频文件 |
| `TmdbSearchFailed` | TMDB 搜索无结果 |

## 模块结构

```
src/Moe/Job/Import/
  Scan.hs              -- importExistingBangumi 入口 + 全部逻辑

src/Moe/Domain/
  File.hs              -- parseFolderName, parseSeasonDir, isBangumiDir
  Bangumi.hs           -- Bangumi, SeasonIndex, TmdbId

src/Moe/Infra/
  Database/Bangumi.hs  -- upsertBangumi, findExistingBangumi
  Database/Tracking.hs -- upsertTracking, getTrackingByBangumi
  Metadata/Effect.hs   -- searchTmdb, getTmdbTvDetail, getTmdbMovieDetail

src/Moe/Web/
  Route/Import.hs      -- POST /api/import/scan handler
```
