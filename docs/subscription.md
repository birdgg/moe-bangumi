# Subscription

RSS 订阅处理流程，自动下载并按媒体服务器规范组织番剧文件。

## 触发方式

RSS 处理由专用 worker 线程 (`rssWorkerThread`) 统一执行，有两种触发来源：

- **定时轮询**：worker 每 30 分钟调用 `getSubscriptionContexts` 查询当天应处理的订阅
- **Handler 推送**：创建/更新 Tracking 时，`triggerSingleSubscription` 将 `RssContext` 写入 `TQueue`，worker 立即消费

两种来源通过 STM `orElse` 组合：队列有数据立即处理，否则等 30 分钟定时器到期后全量轮询。全量轮询后会清空队列以避免重复。

## 整体流程

```mermaid
flowchart TD
    subgraph Worker ["rssWorkerThread"]
        direction TB
        W1[启动时立即全量轮询] --> W2[创建 30 分钟定时器]
        W2 --> W3{STM orElse}
        W3 -->|队列有数据| W4[processSingleFeed]
        W4 --> W3
        W3 -->|定时器到期| W5[pollAndProcessAll]
        W5 -->|flushTQueue| W2
    end

    subgraph Handler ["API Handler"]
        H1[创建/更新 Tracking] --> H2[writeTQueue RssContext]
    end

    H2 -.->|TQueue| W3

    W5 --> B[查询启用 RSS 的 Tracking + Bangumi]
    B --> C[转换为 RssContext 列表]
    C --> D[顺序处理每个 RssContext]

    D --> F[processFeed]
    W4 --> F

    F --> G[fetchRss 获取 RSS Feed]
    G --> H[filterItems 过滤]
    H --> I[parseRawItem 解析为 Episode]
    I --> J[查询 DB 已有 Episode]
    J --> K[processWashing 洗版]
    K --> L{结果}

    L -->|toAdd| M[downloadAndSaveEpisodes]
    L -->|toDelete| N[deleteReplacedTorrents]

    M --> O[addTorrent 到下载器]
    M --> P[upsertEpisode 到 DB]
    N --> Q[deleteTorrents 从下载器移除]

    style H fill:#e8f5e9
    style I fill:#e8f5e9
    style K fill:#e8f5e9
```

> 绿色节点为纯函数，无副作用。

## 单个 RSS 处理 (processFeed)

```mermaid
flowchart LR
    subgraph Pure ["纯函数"]
        direction TB
        Filter[filterItems]
        Parse[parseRawItem]
        Wash[processWashing]
    end

    subgraph Effect ["副作用"]
        direction TB
        Fetch[fetchRss]
        QueryDB[listEpisodesByBangumi]
        Download[addTorrent]
        Delete[deleteTorrents]
        Save[upsertEpisode]
    end

    Fetch -->|"[RawItem]"| Filter
    Filter -->|"[RawItem]"| Parse
    Parse -->|"[Episode]"| Wash
    QueryDB -->|"Map EpisodeNumber Episode"| Wash
    Wash -->|toAdd| Download
    Wash -->|toAdd| Save
    Wash -->|toDelete| Delete
```

## Filter 过滤链

`filterItems` 依次执行三个过滤条件：

```mermaid
flowchart LR
    A["[RawItem]"] --> B["排除匹配全局正则的 item<br/>(合集、批量发布等)"]
    B --> C["排除 pubDate <= lastPubdate<br/>(已处理过的)"]
    C --> D["排除无 pubDate 的 item"]
    D --> E["过滤后 [RawItem]"]
```

默认全局正则过滤：`\d+-\d+`（匹配 `01-13` 等合集格式）

## Washing 洗版逻辑

对每个新解析的 Episode，与数据库中已有 Episode 按集数比较：

```mermaid
flowchart TD
    A[新 Episode] --> B{该集数是否已存在?}
    B -->|不存在| C[加入 toAdd]
    B -->|存在| D{compareGroupPriority}
    D -->|LT 新更优| E[新 Episode 加入 toAdd<br/>旧 Episode 加入 toDelete]
    D -->|GT 旧更优| F[跳过]
    D -->|EQ 相同| G{compareSubtitlePriority}
    G -->|LT 新更优| E
    G -->|EQ 或 GT| F
```

### shouldUpgrade 判断规则

字幕组优先级为主要判断，字幕语言优先级为次级判断（仅在字幕组优先级相同时生效）。

#### 字幕组优先级 (compareGroupPriority)

根据 `groupPriority` 列表（索引越小优先级越高）。每个 Episode 可能有多个字幕组名，取其中**最优索引**（最小值）进行比较。匹配时忽略大小写，并支持别名匹配。

| 已有字幕组 | 新字幕组 | 结果 |
|-----------|---------|------|
| 空 `[]` | 非空 | LT（升级） |
| 任意 | 空 `[]` | GT（不升级） |
| 相同 | 相同 | EQ（进入字幕判断） |
| 均不在列表 | 均不在列表 | EQ（进入字幕判断） |
| 不在列表 | 在列表 | LT（升级） |
| 在列表 | 不在列表 | GT（不升级） |
| 最优索引 = 2 | 最优索引 = 0 | LT（升级） |
| 最优索引 = 0 | 最优索引 = 2 | GT（不升级） |

#### 字幕语言优先级 (compareSubtitlePriority)

根据 `subtitlePriority` 列表。比较时先对字幕列表排序后再匹配。

默认优先级：`[简日, 简, 繁日, 繁]`

| 已有字幕 | 新字幕 | 结果 |
|---------|-------|------|
| 相同（排序后） | 相同（排序后） | EQ（不升级） |
| 不在列表 | 在列表 | LT（升级） |
| 在列表 | 不在列表 | GT（不升级） |
| 索引 = 2 | 索引 = 0 | LT（升级） |
| 索引 = 0 | 索引 = 2 | GT（不升级） |
| 均不在列表 | 均不在列表 | EQ（不升级） |

## 文件命名

下载时通过 `toBangumiFile` 生成符合媒体服务器规范的路径：

```
savePath: 金牌得主 (2025)/Season 01/
fileName: 金牌得主 - S01E03 [喵萌奶茶屋] [LoliHouse].mkv
```

## 模块结构

```
Job/Subscription.hs          -- Re-export 模块
Job/Subscription/
  Types.hs                   -- RssContext 类型定义
  Filter.hs                  -- RSS item 过滤（纯函数）
  Washing.hs                 -- 洗版逻辑（纯函数）
  Download.hs                -- 下载与删除 torrent
  Process.hs                 -- processFeed 流程编排 + triggerSingleSubscription
  Worker.hs                  -- RSS worker 线程（TQueue + 定时轮询）
```
