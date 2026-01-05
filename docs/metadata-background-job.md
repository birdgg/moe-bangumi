# Metadata Background Job 架构

本文档介绍 MoeBangumi 的 Metadata 后台任务系统，采用独立 Actor 模式实现海报下载和 TMDB ID 同步。

## 目录

1. [概述](#概述)
2. [架构设计](#架构设计)
3. [核心组件](#核心组件)
4. [同步流程](#同步流程)
5. [并发控制](#并发控制)
6. [配置参数](#配置参数)
7. [文件位置](#文件位置)

---

## 概述

Metadata Background Job 负责两个核心任务：

1. **Poster 下载** - 将远程海报图片（BGM.tv/TMDB）下载到本地存储
2. **TMDB ID 同步** - 通过日文标题搜索 TMDB，获取 TMDB ID 用于媒体库集成

### 设计目标

- **异步非阻塞** - 后台运行，不影响 API 响应
- **自动恢复** - 系统重启后自动继续未完成的同步
- **资源控制** - 限制并发请求，避免压垮外部 API
- **幂等性** - 重复执行不会产生副作用

### 触发方式

| 触发方式 | 说明 |
|---------|------|
| 定时触发 | 每 1 小时自动执行一次全量同步 |
| 手动触发 | 通过 `MetadataHandle::trigger_sync()` 手动触发 |
| 日历刷新 | `CalendarService::refresh_calendar()` 创建新 Metadata 后触发 |

---

## 架构设计

采用 **Actor 模式**，通过消息队列实现组件解耦：

```
┌─────────────────────────────────────────────────────────────┐
│                       AppState                               │
│  ┌─────────────────┐  ┌─────────────────┐                   │
│  │ MetadataHandle  │  │ MetadataService │                   │
│  │  (对外接口)      │  │  (CRUD + 搜索)   │                   │
│  └────────┬────────┘  └─────────────────┘                   │
└───────────┼─────────────────────────────────────────────────┘
            │ mpsc::channel
            ▼
┌───────────────────────────────────────────────────────────┐
│                    MetadataActor                           │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────────┐    │
│  │ 消息循环     │  │ 定时器       │  │ Runner          │    │
│  │ (recv loop) │  │ (1h interval)│  │ (业务逻辑)       │    │
│  └─────────────┘  └─────────────┘  └────────┬────────┘    │
└─────────────────────────────────────────────┼─────────────┘
                                              │
            ┌─────────────────────────────────┼─────────────┐
            │                                 │             │
            ▼                                 ▼             ▼
    ┌───────────────┐                ┌───────────────┐ ┌─────────┐
    │ PosterService │                │ TMDB Client   │ │ DB Pool │
    │ (图片下载)     │                │ (API 查询)    │ │         │
    └───────────────┘                └───────────────┘ └─────────┘
```

### Actor 模式优势

1. **状态隔离** - Actor 内部状态不被外部直接访问
2. **消息驱动** - 所有操作通过消息触发，天然支持异步
3. **优雅关闭** - 通过 Shutdown 消息实现有序退出
4. **独立生命周期** - 后台任务与 HTTP 服务解耦

---

## 核心组件

### MetadataHandle

对外接口，通过 channel 发送消息给 Actor：

```rust
pub struct MetadataHandle {
    sender: mpsc::Sender<MetadataMessage>,
}

impl MetadataHandle {
    /// 下载单个海报
    pub async fn download(&self, metadata_id: i64, url: String);

    /// 批量下载海报
    pub async fn download_batch(&self, tasks: Vec<(i64, String)>);

    /// 手动触发同步
    pub async fn trigger_sync(&self);

    /// 优雅关闭
    pub async fn shutdown(&self);

    /// 检查 Actor 是否运行中
    pub fn is_running(&self) -> bool;
}
```

### MetadataMessage

Actor 消息类型：

```rust
pub enum MetadataMessage {
    /// 下载单个海报
    DownloadPoster { metadata_id: i64, url: String },

    /// 批量下载海报
    DownloadPosterBatch { tasks: Vec<(i64, String)> },

    /// 触发全量同步
    TriggerSync,

    /// 关闭 Actor
    Shutdown,
}
```

### MetadataActor

Actor 主体，运行在独立 tokio task 中：

```rust
pub struct MetadataActor {
    receiver: mpsc::Receiver<MetadataMessage>,
    runner: MetadataRunner,
}

impl MetadataActor {
    /// 主循环：接收消息 + 定时触发
    pub async fn run(mut self) {
        let mut interval = tokio::time::interval(Duration::from_secs(3600));

        loop {
            tokio::select! {
                Some(msg) = self.receiver.recv() => {
                    match msg {
                        MetadataMessage::Shutdown => break,
                        msg => self.runner.handle(msg).await,
                    }
                }
                _ = interval.tick() => {
                    self.runner.sync_all_metadata().await;
                }
            }
        }
    }
}
```

### PosterService

海报下载服务：

```rust
pub struct PosterService {
    client_provider: Arc<HttpClientService>,
    data_path: PathBuf,
}

impl PosterService {
    /// 下载海报到本地
    /// 返回本地路径如 "/posters/abc123def456.jpg"
    pub async fn download_from_url(&self, url: &str) -> Result<String, PosterError>;
}
```

**下载流程：**

1. 检查是否已是本地路径（`/posters/*`）
2. BGM.tv URL 规范化（移除 `/r/{size}/`）
3. URL 哈希生成文件名（SHA256 前 16 字符）
4. 检查文件是否已存在
5. 确定文件扩展名（从 URL 或 Content-Type）
6. 原子写入（temp file → rename）

---

## 同步流程

### 全量同步 (sync_all_metadata)

```
定时触发 / 手动触发
        │
        ▼
┌───────────────────────────────────────┐
│ MetadataRepository::get_metadata_to_sync()
│   查询需要同步的记录（分块 100 条）
│   条件：
│   - poster_url 是远程 URL（非 /posters/*）
│   - 或 tmdb_id 为空且 title_japanese 非空且 7 天未查询
└───────────────────────────────────────┘
        │
        ▼
┌───────────────────────────────────────┐
│ stream::iter(records).buffer_unordered(5)
│   并发处理每条记录
└───────────────────────────────────────┘
        │
        ├────────────────────────────────┐
        │                                │
        ▼                                ▼
┌─────────────────────┐      ┌─────────────────────┐
│ needs_poster_sync?  │      │ needs_tmdb_sync?    │
│                     │      │                     │
│ poster_url 存在     │      │ tmdb_id 为空        │
│ 且是远程 URL        │      │ 且 title_japanese   │
└─────────┬───────────┘      │ 存在               │
          │                  │ 且 7 天未查询       │
          ▼                  └─────────┬───────────┘
┌─────────────────────┐                │
│ spawn_poster_task() │                ▼
│ - URL 去重检查      │      ┌─────────────────────┐
│ - 信号量限制(5)     │      │ MetadataService::   │
│ - PosterService     │      │   find_tmdb_id()    │
│   .download()       │      │ - TMDB Discover API │
│ - 更新 poster_url   │      │ - 年份匹配(±1)      │
│   为本地路径        │      │ - 更新 tmdb_id      │
└─────────────────────┘      │ - 更新 tmdb_lookup_at│
                             └─────────────────────┘
```

### 同步条件详解

#### Poster 同步条件

```rust
fn needs_poster_sync(metadata: &Metadata) -> bool {
    metadata.poster_url
        .as_ref()
        .map(|url| !url.starts_with("/posters/"))
        .unwrap_or(false)
}
```

即：`poster_url` 存在且不是本地路径。

#### TMDB ID 同步条件

```rust
fn needs_tmdb_sync(metadata: &Metadata) -> bool {
    metadata.tmdb_id.is_none()
        && metadata.title_japanese.is_some()
        && should_retry_tmdb_lookup(metadata.tmdb_lookup_at)
}

fn should_retry_tmdb_lookup(last_lookup: Option<DateTime>) -> bool {
    last_lookup
        .map(|t| Utc::now() - t > Duration::days(7))
        .unwrap_or(true)
}
```

即：`tmdb_id` 为空、有日文标题、且距离上次查询超过 7 天（或从未查询）。

### TMDB ID 查找逻辑

```rust
// MetadataService::find_tmdb_id()

// 1. 使用 TMDB Discover API 搜索
let results = self.tmdb
    .discover_tv()
    .with_original_language("ja")
    .with_first_air_date_year(year)  // 年份精确匹配
    .execute()
    .await?;

// 2. 若无结果，尝试年份 ±1
if results.is_empty() {
    // 尝试 year - 1 和 year + 1
}

// 3. 匹配标题
for result in results {
    if result.original_name == title_japanese {
        return Ok(Some(result.id));
    }
}
```

---

## 并发控制

### 多层并发限制

```
┌─────────────────────────────────────────────────────────────┐
│                    sync_all_metadata()                       │
│                                                              │
│  ┌────────────────────────────────────────────────────────┐ │
│  │ buffer_unordered(5)                                    │ │
│  │ 同时处理 5 条 Metadata 记录                             │ │
│  └────────────────────────────────────────────────────────┘ │
│                           │                                  │
│              ┌────────────┼────────────┐                    │
│              │            │            │                    │
│              ▼            ▼            ▼                    │
│  ┌──────────────┐ ┌──────────────┐ ┌──────────────┐        │
│  │ Poster Task  │ │ Poster Task  │ │ TMDB Query   │        │
│  │ Semaphore(5) │ │ Semaphore(5) │ │ (无额外限制)  │        │
│  └──────────────┘ └──────────────┘ └──────────────┘        │
│                                                              │
└─────────────────────────────────────────────────────────────┘
```

### URL 去重机制

防止同一 URL 被多个任务同时下载：

```rust
struct MetadataRunner {
    /// 正在下载的 URL 集合
    downloading_urls: Arc<Mutex<HashSet<String>>>,
}

impl MetadataRunner {
    async fn spawn_poster_download_task(&self, url: String) {
        // 1. 检查是否已在下载
        {
            let mut urls = self.downloading_urls.lock().unwrap();
            if urls.contains(&url) {
                return; // 跳过重复 URL
            }
            urls.insert(url.clone());
        }

        // 2. RAII Guard 确保下载完成后移除
        let guard = DownloadGuard::new(url.clone(), self.downloading_urls.clone());

        // 3. 执行下载
        // ...

        drop(guard); // 自动从集合中移除
    }
}
```

### 信号量控制

限制同时进行的 HTTP 请求数：

```rust
const POSTER_DOWNLOAD_CONCURRENCY: usize = 5;

struct MetadataRunner {
    poster_semaphore: Arc<Semaphore>,
}

impl MetadataRunner {
    fn new() -> Self {
        Self {
            poster_semaphore: Arc::new(Semaphore::new(POSTER_DOWNLOAD_CONCURRENCY)),
        }
    }

    async fn spawn_poster_download_task(&self) {
        let permit = self.poster_semaphore.acquire().await.unwrap();
        // 执行下载...
        drop(permit); // 释放许可
    }
}
```

### 分块处理

避免一次性加载大量数据到内存：

```rust
// MetadataRepository::get_metadata_to_sync()

const CHUNK_SIZE: i64 = 100;

pub async fn get_metadata_to_sync(pool: &SqlitePool) -> Vec<Metadata> {
    let mut all_records = Vec::new();
    let mut last_id = 0;

    loop {
        let chunk = sqlx::query_as!(
            Metadata,
            r#"
            SELECT * FROM metadata
            WHERE id > ?
              AND (
                (poster_url IS NOT NULL AND poster_url NOT LIKE '/posters/%')
                OR (tmdb_id IS NULL AND title_japanese IS NOT NULL
                    AND (tmdb_lookup_at IS NULL OR tmdb_lookup_at < datetime('now', '-7 days')))
              )
            ORDER BY id
            LIMIT ?
            "#,
            last_id,
            CHUNK_SIZE
        )
        .fetch_all(pool)
        .await?;

        if chunk.is_empty() {
            break;
        }

        last_id = chunk.last().unwrap().id;
        all_records.extend(chunk);
    }

    all_records
}
```

---

## 配置参数

| 参数 | 值 | 位置 | 说明 |
|------|-----|------|------|
| 同步间隔 | 1 小时 | `actor.rs` | 定时触发全量同步的间隔 |
| Poster 并发限制 | 5 | `runner.rs` | 同时下载海报的最大数量 |
| Sync 并发限制 | 5 | `runner.rs` | `buffer_unordered` 的并发数 |
| 分块大小 | 100 条 | `runner.rs` | 每次从数据库查询的记录数 |
| TMDB 冷却期 | 7 天 | `metadata.rs` | TMDB 查询失败后的重试间隔 |
| Poster URL 前缀 | `/posters/` | `poster.rs` | 本地海报路径前缀 |

### 错误类型

```rust
// PosterError
pub enum PosterError {
    Request(reqwest::Error),           // HTTP 请求失败
    Io { operation, path, source },    // 文件操作失败
    InvalidPath(String),               // 无效路径
    HttpStatus(u16),                   // HTTP 状态码错误
    HttpClient(String),                // HTTP 客户端错误
}

// MetadataError
pub enum MetadataError {
    Database(sqlx::Error),             // 数据库错误
    NotFound(i64),                     // 记录不存在
    Bgmtv(bgmtv::BgmtvError),         // BGM.tv API 错误
    Tmdb(tmdb::TmdbError),            // TMDB API 错误
}
```

---

## 文件位置

```
crates/server/src/
├── domain/
│   ├── models/
│   │   └── metadata.rs              # Metadata 数据模型
│   ├── repositories/
│   │   └── metadata.rs              # MetadataRepository (含 get_metadata_to_sync)
│   └── services/
│       └── actors/
│           └── metadata/
│               ├── mod.rs           # 模块导出
│               ├── actor.rs         # MetadataActor 定义
│               ├── actor/
│               │   ├── handle.rs    # MetadataHandle (对外接口)
│               │   └── runner.rs    # MetadataRunner (业务逻辑)
│               ├── poster.rs        # PosterService
│               └── service.rs       # MetadataService (CRUD + TMDB 查询)
└── infra/
    └── state.rs                     # AppState (Actor 初始化)
```

### 关键入口

| 功能 | 入口文件 | 函数/方法 |
|------|---------|----------|
| Actor 启动 | `state.rs` | `create_metadata_actor()` |
| 全量同步 | `runner.rs` | `sync_all_metadata()` |
| 海报下载 | `poster.rs` | `download_from_url()` |
| TMDB 查询 | `service.rs` | `find_tmdb_id()` |
| 同步条件查询 | `metadata.rs` (repo) | `get_metadata_to_sync()` |
