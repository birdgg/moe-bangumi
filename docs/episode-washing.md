# Episode Washing（洗版）机制

本文档介绍 moe-bangumi 的洗版机制，帮助开发者理解系统如何处理同一集的多个资源版本。

## 目录

1. [什么是洗版](#什么是洗版)
2. [优先级算法](#优先级算法)
3. [同一 RSS 源中的处理](#同一-rss-源中的处理)
4. [跨 RSS 源的处理](#跨-rss-源的处理)
5. [关键代码位置](#关键代码位置)
6. [场景示例](#场景示例)

---

## 什么是洗版

**洗版（Washing）** 是指当更高优先级的资源出现时，自动替换已有的低优先级资源。

例如：系统先下载了「喵萌字幕组」的版本，后来「ANi」发布了同一集，如果配置中 ANi 优先级更高，系统会：
1. 删除喵萌的数据库记录和下载任务
2. 创建 ANi 的记录并开始下载

**核心特点：**
- 完全基于优先级，与时间顺序无关
- 数据库操作具有原子性（事务保证）
- 下载器操作是 best-effort（失败不影响数据一致性）

---

## 优先级算法

### 配置结构

优先级通过 `PrioritySettings` 配置，包含两个维度：

```rust
pub struct PrioritySettings {
    /// 字幕组优先级（索引越小优先级越高）
    pub subtitle_groups: Vec<String>,
    /// 语言组合优先级（索引越小优先级越高）
    pub subtitle_language_sets: Vec<SubtitleLanguageSet>,
}
```

**示例配置：**
```rust
subtitle_groups: ["ANi", "喵萌奶茶屋", "桜都字幕组"]
subtitle_language_sets: [
    [Chs],           // 简体中文
    [Chs, Jpn],      // 简日双语
    [Cht],           // 繁体中文
    [Cht, Jpn],      // 繁日双语
]
```

### 评分计算

每个资源被计算为一个 `PriorityScore`：

```rust
pub struct PriorityScore {
    pub group_rank: usize,     // 字幕组排名
    pub language_rank: usize,  // 语言组合排名
}
```

**计算规则：**
- 在配置列表中的位置就是 rank 值（0 = 最高优先级）
- 不在列表中的值为 `usize::MAX`（最低优先级）

**示例：**

| 资源 | group_rank | language_rank | 评分 |
|------|------------|---------------|------|
| ANi + 简日 | 0 | 1 | (0, 1) 最高 |
| ANi + 简体 | 0 | 0 | (0, 0) 最高 |
| 喵萌 + 简日 | 1 | 1 | (1, 1) |
| 未知组 + 简体 | MAX | 0 | (MAX, 0) 最低 |

### 优先级比较

比较时**字幕组权重高于语言**：

```rust
impl Ord for PriorityScore {
    fn cmp(&self, other: &Self) -> Ordering {
        // 先比较字幕组
        match self.group_rank.cmp(&other.group_rank) {
            Ordering::Equal => self.language_rank.cmp(&other.language_rank),
            other => other,
        }
    }
}
```

**比较示例：**
```
(0, 1) < (1, 0)  → ANi+简日 优于 喵萌+简体（字幕组优先）
(0, 0) < (0, 1)  → ANi+简体 优于 ANi+简日（同组看语言）
(1, 0) < (MAX, 0) → 喵萌+简体 优于 未知组+简体
```

### 语言匹配规则

语言组合匹配是**顺序无关的精确匹配**：

```rust
// [Chs, Jpn] 和 [Jpn, Chs] 被视为相同
// [Chs] 和 [Chs, Jpn] 是不同的组合
```

---

## 同一 RSS 源中的处理

当一次 RSS 抓取返回同集的多个版本时，系统在**入库前预过滤**：

```
RSS 返回:
  E05: [ANi] [简日]      → 评分 (0, 1) ✅ 保留
  E05: [喵萌] [简繁日]    → 评分 (1, 4) ❌ 过滤
  E05: [桜都] [繁体]     → 评分 (2, 2) ❌ 过滤
```

**处理流程：**

```
RSS Items
    ↓
解析每个 item（提取集数、字幕组、语言）
    ↓
按集数分组
    ↓
每组只保留最高优先级的一个  ← 预过滤
    ↓
检查数据库是否已有记录
    ↓
创建记录 / 触发洗版 / 跳过
```

**优点：**
- 避免创建不必要的数据库记录
- 减少下载器任务调度
- 一次 RSS 抓取只会为每集产生一个下载任务

**代码位置：** `rss_processing.rs` 的 `filter_by_priority()` 函数

---

## 跨 RSS 源的处理

当不同 RSS 源在不同时间返回同一集时，系统通过 `should_wash()` 判断是否需要洗版：

```rust
pub fn should_wash(&self, existing: &[Torrent], new: &ParseResult) -> bool {
    // 1. 计算新资源的优先级
    let new_score = calculator.calculate_score(&new.to_comparable());

    // 2. 找到现有资源中的最高优先级
    let best_existing_score = existing.iter()
        .map(|t| calculator.calculate_score(&t.to_comparable()))
        .min();  // min = 最高优先级

    // 3. 新资源必须严格高于现有最佳
    new_score < best_existing_score
}
```

**关键规则：**
- 新资源必须**严格高于**现有最佳资源才会触发洗版
- 优先级相等时**不洗版**（保留已有的）
- 来自不同 RSS 源的资源完全平等对待

### 洗版执行流程

```rust
pub async fn wash_episode(&self, params: WashParams) -> Result<Vec<String>> {
    // 步骤 1-3: 数据库事务（原子操作）
    let mut tx = self.db.begin().await?;

    for existing in params.existing_torrents {
        TorrentRepository::delete(&mut tx, existing.id).await?;
    }
    TorrentRepository::create(&mut tx, new_torrent).await?;

    tx.commit().await?;  // 原子提交

    // 步骤 4-5: 下载器操作（best-effort）
    for hash in old_hashes {
        self.downloader.delete_task(&hash);  // 不等待结果
    }
    self.downloader.add_task(new_task);  // 不等待结果

    Ok(old_hashes)
}
```

**事务保证：**
- 删除旧记录和创建新记录在同一事务中
- 任一操作失败则全部回滚
- 下载器操作在事务提交后执行，失败不影响数据库一致性

**注意：下载器失败的影响**

```
┌─────────────────────────────────────────────────────────┐
│  数据库事务提交  →  下载器操作（可能失败）               │
│       ✓                    ✗                           │
│                                                         │
│  结果：torrent 表已更新，但下载器中没有对应任务          │
└─────────────────────────────────────────────────────────┘
```

这是设计上的权衡：
- **优点**：数据库状态始终一致，不会因下载器问题导致事务回滚
- **缺点**：可能出现数据库有记录但下载器无任务的情况

**恢复机制：** `TorrentSyncJob` 定时任务（每 10 分钟）会检测数据库中有记录但下载器中缺失的任务，并自动重新添加。

---

## 关键代码位置

| 文件 | 职责 |
|------|------|
| `core/washing/src/lib.rs` | 优先级算法核心（评分计算、比较逻辑） |
| `core/server/src/services/washing.rs` | 洗版服务（事务管理、下载器协调） |
| `core/server/src/services/rss_processing.rs` | RSS 处理编排（预过滤、触发洗版） |
| `core/server/src/services/scheduler/torrent_sync_job.rs` | 状态同步任务（检测并修复缺失的下载任务） |

### 关键数据结构

```rust
// 洗版参数
pub struct WashParams<'a> {
    pub bangumi_id: i64,
    pub existing_torrents: &'a [Torrent],  // 要替换的旧种子
    pub info_hash: &'a str,
    pub torrent_url: &'a str,
    pub episode: i32,
    pub parse_result: &'a ParseResult,
    pub save_path: &'a str,
    pub rename: &'a str,
}

// 可比较的种子信息
pub struct ComparableTorrent {
    pub subtitle_group: Option<String>,
    pub subtitle_languages: Vec<SubType>,
}
```

---

## 场景示例

### 场景 A：同一 RSS 源返回多个版本

```
08:00 - RSS_Mikan 抓取返回：
  [ANi] 番剧 05 [简日]
  [喵萌] 番剧 05 [简繁日]
  [桜都] 番剧 05 [繁体]

处理：
  1. 预过滤：只保留 ANi 版本
  2. 检查数据库：第 5 集无记录
  3. 创建 ANi 记录，添加下载任务

结果：只有 ANi 版本入库
```

### 场景 B：跨时间洗版

```
08:00 - RSS_A 返回 [喵萌] [简繁日]
  → 数据库无记录，创建喵萌记录
  → 添加喵萌下载任务

12:00 - RSS_B 返回 [ANi] [简日]
  → should_wash: (0,1) < (1,4)? 是 ✅
  → 执行洗版：
    - 事务内删除喵萌记录
    - 事务内创建 ANi 记录
    - 事务提交
    - 删除喵萌下载任务
    - 添加 ANi 下载任务

16:00 - RSS_A 再次返回 [喵萌] [简繁日]
  → should_wash: (1,4) < (0,1)? 否 ❌
  → 跳过（现有优先级更高）

结果：最终保留 ANi 版本
```

### 场景 C：优先级相等

```
08:00 - RSS_A 返回 [ANi] [简日]
  → 创建记录

12:00 - RSS_B 也返回 [ANi] [简日]
  → should_wash: (0,1) < (0,1)? 否 ❌
  → 跳过（优先级相等，保留已有）

结果：保留最先到达的版本
```

### 场景 D：未配置的字幕组

```
配置：subtitle_groups = ["ANi", "喵萌"]

08:00 - RSS 返回 [未知字幕组] [简体]
  → 评分 (MAX, 0)
  → 创建记录（无更好选择）

12:00 - RSS 返回 [喵萌] [简繁日]
  → should_wash: (1,4) < (MAX,0)? 是 ✅
  → 洗版替换

结果：配置内的字幕组总是优于未配置的
```

---

## 总结

洗版机制的设计原则：

1. **优先级驱动** - 完全基于配置的优先级，不看时间
2. **字幕组优先** - 字幕组排名权重高于语言组合
3. **严格比较** - 只有严格更高优先级才触发洗版
4. **预过滤** - 同一 RSS 源内先过滤，减少冗余操作
5. **原子保证** - 数据库操作在事务中，保证一致性
6. **容错设计** - 下载器失败不影响数据库状态
