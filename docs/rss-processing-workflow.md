# RSS 处理工作流架构指南

## 概述

RSS 处理系统负责从动画 RSS 源（蜜柑、Nyaa）自动发现和下载新番。核心特性包括：增量更新、正则过滤、优先级洗版。

## 架构层次

```
┌─────────────────────────────────────────────────────────────┐
│                        触发层                                │
│  ┌──────────────────┐    ┌──────────────────────────────┐   │
│  │   RssFetchJob    │    │      BangumiService          │   │
│  │   (每小时定时)    │    │  create() / update()        │   │
│  │                  │    │  (新增RSS时触发后台处理)      │   │
│  └────────┬─────────┘    └──────────────┬───────────────┘   │
└───────────┼─────────────────────────────┼───────────────────┘
            │                             │
            ▼                             ▼
┌─────────────────────────────────────────────────────────────┐
│                      核心处理层                              │
│  ┌────────────────────────────────────────────────────────┐ │
│  │              RssProcessingService                      │ │
│  │  • process_single() - 处理单条RSS                      │ │
│  │  • process_batch()  - 并发处理5条RSS                     │ │
│  │  • spawn_background() - 异步后台处理                   │ │
│  └────────────────────────────────────────────────────────┘ │
└─────────────────────────────────────────────────────────────┘
            │
            ▼
┌─────────────────────────────────────────────────────────────┐
│                       依赖服务层                             │
│  ┌──────────────┐ ┌──────────────┐ ┌──────────────────────┐ │
│  │  RssClient   │ │    Parser    │ │   WashingService     │ │
│  │  (HTTP获取)  │ │  (文件名解析) │ │   (优先级洗版)       │ │
│  └──────────────┘ └──────────────┘ └──────────────────────┘ │
│  ┌──────────────────────────────────────────────────────┐   │
│  │              DownloaderService (Actor模式)            │   │
│  │              添加/删除下载任务                         │   │
│  └──────────────────────────────────────────────────────┘   │
└─────────────────────────────────────────────────────────────┘
            │
            ▼
┌─────────────────────────────────────────────────────────────┐
│                       数据访问层                             │
│  ┌──────────────────┐    ┌──────────────────────────────┐   │
│  │  RssRepository   │    │     TorrentRepository        │   │
│  └──────────────────┘    └──────────────────────────────┘   │
└─────────────────────────────────────────────────────────────┘
```

## 触发条件

| 场景 | 是否触发 | 说明 |
|------|----------|------|
| 创建 Bangumi | ✅ | 所有 RSS 订阅立即后台处理 |
| 更新 Bangumi + 新增 RSS | ✅ | 仅新增的 RSS 触发后台处理 |
| 更新 Bangumi + 修改现有 RSS | ❌ | 等下次定时任务 |
| 定时任务 (每小时) | ✅ | 处理所有 enabled=true 的 RSS |

## 数据流程

```
RSS源 ──► 获取 ──► 过滤 ──► 解析 ──► 去重/洗版 ──► 下载
         HTTP      正则     文件名     数据库        qBittorrent
         缓存      匹配     提取       检查          任务
```

### 详细流程

```
1. 获取阶段
   RssClient::fetch_conditional()
   ├── 发送条件请求 (If-None-Match / If-Modified-Since)
   ├── HTTP 304 → 跳过（无更新）
   └── HTTP 200 → 解析XML，提取 RssItem[]

2. 过滤阶段
   ├── pubDate过滤: 只处理比 last_pub_date 更新的项
   ├── include_filters: 正则白名单（空=全部通过）
   └── exclude_filters: 正则黑名单

3. 解析阶段
   Parser::parse(title)
   └── 提取: episode, subtitle_group, subtitle_language, resolution

4. 优先级预过滤
   filter_by_priority()
   └── 对每集只保留一个最高优先级项目

5. 去重/洗版
   process_item()
   ├── info_hash已存在 → 跳过
   ├── 该集无种子 → 直接添加
   └── 该集有种子 → 比较优先级
       ├── 新种子优先级更高 → 洗版（删旧添新）
       └── 否则 → 跳过

6. 添加下载
   create_and_add_task()
   ├── 创建Torrent记录（事务）
   ├── 生成Plex/Jellyfin兼容文件名
   └── 调用 downloader.add_task()
```

## 核心数据模型

### RSS 订阅

```
rss表
├── id, bangumi_id
├── url                    # RSS源地址
├── enabled                # 是否启用
├── include_filters[]      # 包含正则
├── exclude_filters[]      # 排除正则
├── subtitle_group         # 字幕组名称
├── etag, last_modified    # HTTP缓存
└── last_pub_date          # 增量更新标记
```

### Torrent 记录

```
torrent表
├── id, bangumi_id, rss_id
├── info_hash (UNIQUE)     # 去重依据
├── torrent_url
├── episode_number
├── subtitle_group         # 用于优先级比较
├── subtitle_language[]    # 语言类型
└── resolution             # 分辨率
```

## 洗版机制

洗版 = 用更高优先级的资源替换已下载的低优先级版本

### 优先级计算

```
1. 字幕组优先级（用户配置顺序）
   例: ["ANi", "喵萌", "FZSD"] → ANi > 喵萌 > FZSD

2. 语言优先级
   简日 > 繁体 > 其他
```

### 洗版流程

[washing](./episode-washing.md)

```
should_wash() = 新种子优先级 > 现有最佳优先级

wash_episode() (事务):
├── 删除旧Torrent记录
├── 从下载器删除旧任务
├── 创建新Torrent记录
└── 添加新下载任务
```

## 关键优化

| 优化点 | 实现方式 |
|--------|----------|
| HTTP缓存 | ETag + Last-Modified → 304时跳过处理 |
| 增量更新 | pubDate追踪 → 只处理新发布项 |
| 并发处理 | `buffer_unordered(5)` 限制并发数 |
| O(1)查询 | HashMap缓存现有种子 |
| 原子操作 | 事务确保记录和下载一致 |

### 并发控制

```
RSS_FETCH_CONCURRENCY = 5

实现: buffer_unordered(5) - 最多同时处理5个RSS源
位置: rss_processing.rs:10
```

## 关键文件

| 功能 | 位置 |
|------|------|
| RSS客户端 | `crates/rss/src/client.rs` |
| RSS解析器 | `crates/rss/src/parsers/` |
| 核心处理 | `crates/server/src/services/rss_processing.rs` |
| 洗版服务 | `crates/server/src/services/washing.rs` |
| 定时任务 | `crates/server/src/services/scheduler/rss_fetch_job.rs` |
| 数据模型 | `crates/server/src/models/rss.rs` |
| 数据库操作 | `crates/server/src/repositories/rss.rs` |
