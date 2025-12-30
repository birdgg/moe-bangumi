# 番剧洗版优先级系统架构设计

## 概述

本文档描述了基于自定义优先级的番剧洗版系统重构方案。该系统允许用户配置字幕组、字幕语种、分辨率的优先级顺序，自动选择最优资源并支持实时洗版。

## 需求确认

1. **洗版时机**：实时洗版 - 不管下载状态，有更高优先级资源就立即替换
2. **配置范围**：全局配置 - 所有番剧共用一套优先级规则
3. **优先级顺序**：固定顺序 - 字幕组 > 字幕语种 > 分辨率
4. **未知属性**：视为最低优先级
5. **多种子比较**：用现有种子中优先级最高的一个与新资源比较
6. **is_primary**：完全移除，使用新优先级系统替代
7. **未配置项**：视为最低优先级

---

## 架构设计

### 一、核心原则

1. **单一职责**：优先级计算独立成模块，与业务逻辑解耦
2. **可测试性**：优先级比较逻辑完全独立，易于单元测试
3. **可扩展性**：新增优先级维度或调整顺序只需修改配置
4. **类型安全**：使用强类型而非字符串比较

---

### 二、模块设计

#### 1. 新增 `priority` 模块

**位置**：`crates/server/src/priority.rs`

**职责**：
- 定义优先级规则配置
- 实现优先级计算逻辑
- 提供种子比较接口

**核心类型**：

```rust
/// 优先级配置（存储在 Settings 中）
pub struct PriorityConfig {
    /// 字幕组优先级列表（越前越优先）
    pub subtitle_groups: Vec<String>,
    /// 字幕语种优先级列表（越前越优先）
    pub subtitle_languages: Vec<String>,
    /// 分辨率优先级列表（越前越优先）
    pub resolutions: Vec<String>,
}

/// 种子优先级评分（用于比较）
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct PriorityScore {
    pub group_rank: usize,      // 字幕组排名（越小越优先）
    pub language_rank: usize,   // 字幕语种排名
    pub resolution_rank: usize, // 分辨率排名
}

/// 可比较的种子信息（从 ParseResult 提取）
pub struct ComparableTorrent {
    pub subtitle_group: Option<String>,
    pub sub_type: Option<String>,  // 字幕语种
    pub resolution: Option<String>,
}

/// 优先级计算器
pub struct PriorityCalculator {
    config: PriorityConfig,
}

impl PriorityCalculator {
    pub fn new(config: PriorityConfig) -> Self;

    /// 计算单个种子的优先级评分
    pub fn calculate_score(&self, torrent: &ComparableTorrent) -> PriorityScore;

    /// 比较两个种子优先级（返回 true 表示 new 优先级更高）
    pub fn is_higher_priority(&self, new: &ComparableTorrent, existing: &ComparableTorrent) -> bool;

    /// 从多个现有种子中找出优先级最高的
    pub fn find_best(&self, torrents: &[ComparableTorrent]) -> Option<&ComparableTorrent>;
}
```

**排名逻辑**：
- 配置数组中的索引即为排名（索引越小排名越高）
- 未配置的属性统一给予 `usize::MAX` 排名（最低优先级）
- `None` 值（解析失败）也给予 `usize::MAX` 排名

---

#### 2. 扩展 `Settings` 模型

**位置**：`crates/server/src/models/settings.rs`

**新增字段**：

```rust
#[derive(Debug, Clone, Serialize, Deserialize, ToSchema)]
pub struct Settings {
    #[serde(default)]
    pub downloader: DownloaderSettings,
    #[serde(default)]
    pub filter: FilterSettings,
    #[serde(default)]
    pub proxy: ProxySettings,
    #[serde(default)]
    pub notification: NotificationSettings,

    // 新增：优先级配置
    #[serde(default)]
    pub priority: PrioritySettings,
}

/// 优先级配置
#[derive(Debug, Clone, Serialize, Deserialize, ToSchema)]
pub struct PrioritySettings {
    /// 字幕组优先级列表（越前越优先）
    #[serde(default = "PrioritySettings::default_subtitle_groups")]
    pub subtitle_groups: Vec<String>,

    /// 字幕语种优先级列表（越前越优先）
    #[serde(default = "PrioritySettings::default_subtitle_languages")]
    pub subtitle_languages: Vec<String>,

    /// 分辨率优先级列表（越前越优先）
    #[serde(default = "PrioritySettings::default_resolutions")]
    pub resolutions: Vec<String>,
}

impl Default for PrioritySettings {
    fn default() -> Self {
        Self {
            subtitle_groups: Self::default_subtitle_groups(),
            subtitle_languages: Self::default_subtitle_languages(),
            resolutions: Self::default_resolutions(),
        }
    }
}

impl PrioritySettings {
    fn default_subtitle_groups() -> Vec<String> {
        vec!["ANi".to_string(), "喵萌奶茶屋".to_string()]
    }

    fn default_subtitle_languages() -> Vec<String> {
        vec!["简日".to_string(), "简体".to_string(), "繁日".to_string()]
    }

    fn default_resolutions() -> Vec<String> {
        vec!["2160P".to_string(), "1080P".to_string(), "720P".to_string()]
    }

    /// 转换为 PriorityConfig
    pub fn to_config(&self) -> PriorityConfig {
        PriorityConfig {
            subtitle_groups: self.subtitle_groups.clone(),
            subtitle_languages: self.subtitle_languages.clone(),
            resolutions: self.resolutions.clone(),
        }
    }
}

// UpdateSettings 对应更新
#[derive(Debug, Clone, Default, Deserialize, ToSchema)]
pub struct UpdateSettings {
    // ... 现有字段
    #[serde(default)]
    pub priority: Option<UpdatePrioritySettings>,
}

#[derive(Debug, Clone, Default, Deserialize, ToSchema)]
pub struct UpdatePrioritySettings {
    /// 更新字幕组优先级列表（完全替换）
    #[serde(default)]
    pub subtitle_groups: Option<Vec<String>>,

    /// 更新字幕语种优先级列表（完全替换）
    #[serde(default)]
    pub subtitle_languages: Option<Vec<String>>,

    /// 更新分辨率优先级列表（完全替换）
    #[serde(default)]
    pub resolutions: Option<Vec<String>>,
}
```

---

#### 3. 扩展 `Torrent` 模型 - 存储解析信息

**位置**：`crates/server/src/models/torrent.rs`

**新增字段**：

```rust
#[derive(Debug, Clone, Serialize, Deserialize, ToSchema)]
pub struct Torrent {
    pub id: i64,
    pub created_at: chrono::DateTime<chrono::Utc>,
    pub updated_at: chrono::DateTime<chrono::Utc>,
    pub bangumi_id: i64,
    pub rss_id: Option<i64>,
    pub info_hash: String,
    pub torrent_url: String,
    pub episode_number: Option<i32>,

    // 新增：解析信息（用于优先级比较）
    pub subtitle_group: Option<String>,    // 字幕组
    pub subtitle_language: Option<String>, // 字幕语种（sub_type）
    pub resolution: Option<String>,        // 分辨率
}

#[derive(Debug, Clone, Serialize, Deserialize, ToSchema)]
pub struct CreateTorrent {
    pub bangumi_id: i64,
    pub rss_id: Option<i64>,
    pub info_hash: String,
    pub torrent_url: String,
    pub episode_number: Option<i32>,

    // 新增：解析信息
    pub subtitle_group: Option<String>,
    pub subtitle_language: Option<String>,
    pub resolution: Option<String>,
}

impl Torrent {
    /// 转换为可比较的种子信息
    pub fn to_comparable(&self) -> ComparableTorrent {
        ComparableTorrent {
            subtitle_group: self.subtitle_group.clone(),
            sub_type: self.subtitle_language.clone(),
            resolution: self.resolution.clone(),
        }
    }
}
```

**数据库变更**（修改 `migrations/001_init.sql`）：

```sql
-- 在 torrent 表中添加解析信息字段
-- subtitle_group: 字幕组名称
-- subtitle_language: 字幕语种
-- resolution: 视频分辨率

CREATE TABLE IF NOT EXISTS torrent (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    created_at DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,

    bangumi_id INTEGER NOT NULL REFERENCES bangumi(id) ON DELETE CASCADE,
    rss_id INTEGER REFERENCES rss(id) ON DELETE SET NULL,

    info_hash TEXT NOT NULL,
    torrent_url TEXT NOT NULL,
    episode_number INTEGER,

    -- 新增：解析信息字段
    subtitle_group TEXT,
    subtitle_language TEXT,
    resolution TEXT
);

-- 创建索引（可选，用于优化查询）
CREATE INDEX IF NOT EXISTS idx_torrent_subtitle_group ON torrent(subtitle_group);
CREATE INDEX IF NOT EXISTS idx_torrent_resolution ON torrent(resolution);
```

---

#### 4. 重构 `rss_processing.rs`

**位置**：`crates/server/src/services/rss_processing.rs`

**关键变更**：

```rust
use crate::priority::{PriorityCalculator, ComparableTorrent};

impl RssProcessingService {
    /// 处理单个 RSS item
    async fn process_item(
        &self,
        rss: &Rss,
        ctx: &ProcessingContext,
        lookup: &TorrentLookup,
        item: RssItem,
        episode: i32,
    ) {
        let title = item.title();
        let info_hash = item.info_hash();
        let torrent_url = item.torrent_url();

        // 跳过已存在的种子
        if lookup.existing_hashes.contains(info_hash) {
            tracing::debug!("Skipping existing torrent: {}", title);
            return;
        }

        // 解析新种子的属性
        let parse_result = match self.parser.parse(title) {
            Ok(result) => result,
            Err(e) => {
                tracing::warn!("Failed to parse title '{}': {}", title, e);
                return;
            }
        };

        // 检查是否需要洗版
        if let Some(existing_torrents) = lookup.episodes_map.get(&episode) {
            if !self.should_replace_existing(rss, existing_torrents, &parse_result).await {
                tracing::debug!(
                    "[{}] Skipping E{}: existing torrent has higher priority",
                    rss.title,
                    episode
                );
                return;
            }

            // 执行洗版
            self.wash_episode(rss, ctx, existing_torrents, info_hash, torrent_url, episode, &parse_result)
                .await;
        } else {
            // 新集数，直接创建
            self.create_and_add_task(rss, ctx, info_hash, torrent_url, episode, &parse_result)
                .await;
        }
    }

    /// 判断是否应该替换现有种子（新优先级逻辑）
    async fn should_replace_existing(
        &self,
        rss: &Rss,
        existing_torrents: &[Torrent],
        new_parse_result: &ParseResult,
    ) -> bool {
        // 构建优先级计算器
        let settings = self.settings.get();
        let priority_config = settings.priority.to_config();
        let calculator = PriorityCalculator::new(priority_config);

        // 新种子的可比较信息
        let new_comparable = ComparableTorrent {
            subtitle_group: new_parse_result.subtitle_group.clone(),
            sub_type: new_parse_result.sub_type.clone(),
            resolution: new_parse_result.resolution.clone(),
        };

        // 找出现有种子中优先级最高的
        let existing_comparables: Vec<ComparableTorrent> = existing_torrents
            .iter()
            .map(|t| t.to_comparable())
            .collect();

        let best_existing = match calculator.find_best(&existing_comparables) {
            Some(best) => best,
            None => return true, // 没有现有种子，应该添加
        };

        // 比较新旧种子优先级
        calculator.is_higher_priority(&new_comparable, best_existing)
    }

    /// 创建种子记录并添加下载任务（带解析信息）
    async fn create_and_add_task(
        &self,
        rss: &Rss,
        ctx: &ProcessingContext,
        info_hash: &str,
        torrent_url: &str,
        episode: i32,
        parse_result: &ParseResult,
    ) {
        if let Err(e) = TorrentRepository::create(
            &self.db,
            CreateTorrent {
                bangumi_id: rss.bangumi_id,
                rss_id: Some(rss.id),
                info_hash: info_hash.to_string(),
                torrent_url: torrent_url.to_string(),
                episode_number: Some(episode),
                // 新增：存储解析信息
                subtitle_group: parse_result.subtitle_group.clone(),
                subtitle_language: parse_result.sub_type.clone(),
                resolution: parse_result.resolution.clone(),
            },
        )
        .await
        {
            tracing::error!("[{}] Database error: {}", rss.title, e);
            return;
        }

        self.add_download_task(rss, ctx, info_hash, torrent_url, episode)
            .await;
    }

    /// 洗版：删除旧种子，创建新种子（带解析信息）
    async fn wash_episode(
        &self,
        rss: &Rss,
        ctx: &ProcessingContext,
        existing_torrents: &[Torrent],
        info_hash: &str,
        torrent_url: &str,
        episode: i32,
        parse_result: &ParseResult,
    ) {
        tracing::info!(
            "[{}] Washing E{}: replacing {} torrents with higher priority resource",
            rss.title,
            episode,
            existing_torrents.len()
        );

        // 收集旧种子哈希
        let old_hashes: Vec<String> = existing_torrents
            .iter()
            .map(|t| t.info_hash.clone())
            .collect();

        // 事务：删除旧 + 创建新
        let mut tx = match self.db.begin().await {
            Ok(tx) => tx,
            Err(e) => {
                tracing::error!("[{}] Failed to begin transaction: {}", rss.title, e);
                return;
            }
        };

        // 删除现有种子
        for existing in existing_torrents {
            if let Err(e) = TorrentRepository::delete_with_executor(&mut *tx, existing.id).await {
                tracing::error!("[{}] Failed to delete torrent: {}", rss.title, e);
                return;
            }
        }

        // 创建新种子（带解析信息）
        let new_torrent = CreateTorrent {
            bangumi_id: rss.bangumi_id,
            rss_id: Some(rss.id),
            info_hash: info_hash.to_string(),
            torrent_url: torrent_url.to_string(),
            episode_number: Some(episode),
            subtitle_group: parse_result.subtitle_group.clone(),
            subtitle_language: parse_result.sub_type.clone(),
            resolution: parse_result.resolution.clone(),
        };

        if let Err(e) = TorrentRepository::create_with_executor(&mut *tx, new_torrent).await {
            tracing::error!("[{}] Failed to create new torrent: {}", rss.title, e);
            return;
        }

        // 提交事务
        if let Err(e) = tx.commit().await {
            tracing::error!("[{}] Failed to commit transaction: {}", rss.title, e);
            return;
        }

        // 清理下载器中的旧任务
        for hash in &old_hashes {
            self.delete_from_downloader(hash, rss).await;
        }

        // 添加新下载任务
        self.add_download_task(rss, ctx, info_hash, torrent_url, episode)
            .await;
    }
}
```

**删除的逻辑**：
- 完全移除 `is_primary` 相关的判断逻辑
- 删除 `should_override_existing` 方法（被 `should_replace_existing` 替代）
- 简化 `process_batch` 移除 primary/backup 分离

---

### 三、前端 UI 设计

#### 1. 数据结构

**API 响应**（Settings）：

```typescript
interface Settings {
  downloader: DownloaderSettings;
  filter: FilterSettings;
  proxy: ProxySettings;
  notification: NotificationSettings;

  // 新增：优先级配置
  priority: PrioritySettings;
}

interface PrioritySettings {
  subtitle_groups: string[];      // ["ANi", "喵萌奶茶屋"]
  subtitle_languages: string[];   // ["简日", "简体", "繁日"]
  resolutions: string[];          // ["2160P", "1080P", "720P"]
}
```

#### 2. UI 组件设计

**位置**：`web/src/features/settings/components/priority-section.tsx`

**设计风格**：可爱萌系番剧风格，使用拖拽排序

```tsx
import { DndContext, closestCenter, DragEndEvent } from '@dnd-kit/core';
import { SortableContext, useSortable, verticalListSortingStrategy } from '@dnd-kit/sortable';
import { CSS } from '@dnd-kit/utilities';

interface PrioritySectionProps {
  form: SettingsFormInstance;
}

export function PrioritySection({ form }: PrioritySectionProps) {
  return (
    <section className="space-y-6">
      {/* 标题说明 */}
      <div className="rounded-2xl bg-gradient-to-br from-chart-1/5 to-chart-3/5 p-4 border border-chart-1/20">
        <div className="flex items-center gap-2 mb-2">
          <span className="text-xl">✦</span>
          <h3 className="font-semibold text-chart-1">洗版优先级配置</h3>
        </div>
        <p className="text-sm text-muted-foreground">
          拖动调整优先级顺序，越靠前优先级越高。未配置的属性视为最低优先级。
        </p>
      </div>

      {/* 字幕组优先级 */}
      <PriorityList
        title="字幕组优先级"
        icon="♡"
        fieldName="priority.subtitle_groups"
        form={form}
        placeholder="添加字幕组..."
      />

      {/* 字幕语种优先级 */}
      <PriorityList
        title="字幕语种优先级"
        icon="✨"
        fieldName="priority.subtitle_languages"
        form={form}
        placeholder="添加字幕语种..."
      />

      {/* 分辨率优先级 */}
      <PriorityList
        title="分辨率优先级"
        icon="★"
        fieldName="priority.resolutions"
        form={form}
        placeholder="添加分辨率..."
        suggestions={["2160P", "1080P", "720P", "480P"]}
      />
    </section>
  );
}
```

**UI 特点**：
- 可爱风格：渐变背景、圆角卡片、柔和阴影
- 拖拽排序：使用 `@dnd-kit` 实现直观的优先级调整
- 视觉反馈：拖拽时的阴影效果、优先级数字标签
- 快捷操作：快捷添加建议、回车键添加

---

### 四、实现路线图

#### Phase 1: 核心优先级模块
- [ ] 创建 `crates/server/src/priority.rs` 模块
  - [ ] 定义 `PriorityConfig`、`PriorityScore`、`ComparableTorrent`
  - [ ] 实现 `PriorityCalculator` 及其方法
  - [ ] 编写单元测试

#### Phase 2: 数据模型扩展
- [ ] 扩展 `Settings` 模型
  - [ ] 添加 `PrioritySettings` 结构
  - [ ] 添加 `UpdatePrioritySettings` 结构
  - [ ] 更新 `Settings::merge` 方法
- [ ] 扩展 `Torrent` 模型
  - [ ] 添加解析信息字段
  - [ ] 添加 `to_comparable` 方法
- [ ] 数据库变更
  - [ ] 修改 `001_init.sql` 添加新字段
  - [ ] 删除并重建 `moe.db`

#### Phase 3: RSS 处理重构
- [ ] 重构 `rss_processing.rs`
  - [ ] 移除 `is_primary` 相关逻辑
  - [ ] 实现 `should_replace_existing` 方法
  - [ ] 更新 `process_item`、`create_and_add_task`、`wash_episode`
  - [ ] 在创建种子时存储解析信息

#### Phase 4: Repository 更新
- [ ] 更新 `TorrentRepository`
  - [ ] 修改 `create` 和 `create_with_executor` 支持新字段
  - [ ] 修改查询逻辑支持新字段
  - [ ] 新增 `get_existing_hashes` 方法（批量检查 info_hash）
  - [ ] 新增 `get_by_episodes` 方法（按集数批量获取种子）

#### Phase 5: 前端 UI
- [ ] 安装 `@dnd-kit/core` 和 `@dnd-kit/sortable`
- [ ] 创建优先级设置组件
  - [ ] 实现拖拽排序列表
  - [ ] 实现添加/删除功能
  - [ ] 集成到设置页面
- [ ] API 集成
  - [ ] 重新生成 API 客户端（`just web-gen-api`）
  - [ ] 实现保存/加载优先级配置

#### Phase 6: 测试与优化
- [ ] 端到端测试
  - [ ] 测试洗版逻辑（不同优先级场景）
  - [ ] 测试前端拖拽交互
- [ ] 性能优化

---

### 五、优先级计算示例

#### 示例场景

**配置**：
```json
{
  "subtitle_groups": ["ANi", "喵萌奶茶屋", "桜都字幕组"],
  "subtitle_languages": ["简日", "简体", "繁日"],
  "resolutions": ["2160P", "1080P", "720P"]
}
```

**现有种子**：
```
集数 5:
  - Torrent A: [桜都字幕组] 简体 1080P (group_rank=2, language_rank=1, resolution_rank=1)
  - Torrent B: [喵萌奶茶屋] 繁日 720P (group_rank=1, language_rank=2, resolution_rank=2)
```

**新种子**：
```
[ANi] 简日 1080P (group_rank=0, language_rank=0, resolution_rank=1)
```

**比较过程**：
1. 找出现有种子中优先级最高的：Torrent B（字幕组优先级更高）
2. 比较新旧种子：
   - 字幕组：ANi(0) > 喵萌奶茶屋(1) ✅
   - 新种子优先级更高，执行洗版

**洗版结果**：删除 Torrent A 和 B，创建新种子

---

### 六、关键设计决策

#### 1. 为什么存储解析信息到数据库？

**决策**：在 `torrent` 表中添加 `subtitle_group`、`subtitle_language`、`resolution` 字段

**理由**：
- **避免重复解析**：每次比较时无需重新解析文件名（性能优化）
- **数据一致性**：解析逻辑升级后，旧种子仍保留原始解析结果
- **便于查询**：可直接通过 SQL 查询特定字幕组/分辨率的种子
- **审计追踪**：可清楚看到每个种子的属性，便于调试

#### 2. 为什么使用 `usize` 排名而非硬编码分数？

**决策**：使用配置数组索引作为排名，未配置给 `usize::MAX`

**理由**：
- **简单直观**：配置即排名，用户无需理解复杂的评分系统
- **灵活扩展**：新增维度只需添加配置数组
- **无冲突**：`usize::MAX` 确保未配置项始终最低

#### 3. 为什么完全移除 `is_primary`？

**决策**：用优先级系统完全替代主备 RSS 概念

**理由**：
- **更灵活**：不再局限于"主备"二元关系，支持多 RSS 多维度比较
- **更直观**：用户只需关注"优先级规则"，无需理解"主备"语义
- **简化逻辑**：移除复杂的主备判断分支

---

### 七、性能优化：批量预过滤

#### 问题分析

现有的 RSS 处理逻辑在每次处理时都需要加载该 bangumi 下的所有种子记录：

```rust
// 现有方式：加载所有种子
let existing_torrents = TorrentRepository::get_by_bangumi_id(&self.db, rss.bangumi_id).await?;
let existing_hashes: HashSet<String> = existing_torrents.iter().map(|t| t.info_hash.clone()).collect();
```

**问题**：
- RSS 返回的大部分 item 可能都已经处理过
- 每次都加载所有种子记录，浪费数据库资源
- 随着种子数量增加，性能会逐渐下降

#### 优化方案：批量预过滤

在解析 RSS 后，先用一次批量查询过滤掉已存在的 info_hash：

```rust
/// 处理单个 RSS 订阅（优化版）
pub async fn process_single(&self, rss: &Rss, global_exclude_filters: &[String]) {
    // 1. 准备上下文
    let Some(ctx) = self.prepare_context(rss).await else { return };

    // 2. 获取并解析 RSS items
    let Some(parsed_items) = self.fetch_and_parse_items(rss, global_exclude_filters, &ctx).await else { return };

    // 3. 提取所有 info_hash
    let all_hashes: Vec<&str> = parsed_items
        .iter()
        .map(|(item, _)| item.info_hash())
        .collect();

    // 4. 批量查询已存在的 hash（单次 SQL 查询）
    let existing_hashes = match TorrentRepository::get_existing_hashes(
        &self.db,
        rss.bangumi_id,
        &all_hashes,
    ).await {
        Ok(hashes) => hashes,
        Err(e) => {
            tracing::error!("[{}] Failed to check existing hashes: {}", rss.title, e);
            return;
        }
    };

    // 5. 过滤掉已存在的 items
    let new_items: Vec<_> = parsed_items
        .into_iter()
        .filter(|(item, _)| !existing_hashes.contains(item.info_hash()))
        .collect();

    if new_items.is_empty() {
        tracing::debug!("[{}] No new items to process", rss.title);
        return;
    }

    // 6. 只对需要处理的 items 加载 episode 信息
    let episodes_to_check: Vec<i32> = new_items
        .iter()
        .map(|(_, episode)| *episode)
        .collect::<std::collections::HashSet<_>>()
        .into_iter()
        .collect();

    // 7. 按需加载现有种子（只加载相关集数）
    let existing_by_episode = match TorrentRepository::get_by_episodes(
        &self.db,
        rss.bangumi_id,
        &episodes_to_check,
    ).await {
        Ok(torrents) => torrents,
        Err(e) => {
            tracing::error!("[{}] Failed to load existing torrents: {}", rss.title, e);
            return;
        }
    };

    // 8. 处理每个新 item
    for (item, episode) in new_items {
        self.process_item_with_existing(rss, &ctx, &existing_by_episode, item, episode).await;
    }
}
```

#### 新增 Repository 方法

```rust
// crates/server/src/repositories/torrent.rs

impl TorrentRepository {
    /// 批量检查 info_hash 是否存在（返回已存在的 hash 集合）
    pub async fn get_existing_hashes(
        pool: &SqlitePool,
        bangumi_id: i64,
        hashes: &[&str],
    ) -> Result<HashSet<String>> {
        if hashes.is_empty() {
            return Ok(HashSet::new());
        }

        // 构建 IN 子句的占位符
        let placeholders: Vec<String> = (0..hashes.len())
            .map(|i| format!("${}", i + 2))
            .collect();
        let in_clause = placeholders.join(", ");

        let query = format!(
            "SELECT info_hash FROM torrent WHERE bangumi_id = $1 AND info_hash IN ({})",
            in_clause
        );

        let mut query_builder = sqlx::query_scalar::<_, String>(&query)
            .bind(bangumi_id);

        for hash in hashes {
            query_builder = query_builder.bind(*hash);
        }

        let existing: Vec<String> = query_builder.fetch_all(pool).await?;
        Ok(existing.into_iter().collect())
    }

    /// 按集数批量获取种子（用于洗版检查）
    pub async fn get_by_episodes(
        pool: &SqlitePool,
        bangumi_id: i64,
        episodes: &[i32],
    ) -> Result<HashMap<i32, Vec<Torrent>>> {
        if episodes.is_empty() {
            return Ok(HashMap::new());
        }

        let placeholders: Vec<String> = (0..episodes.len())
            .map(|i| format!("${}", i + 2))
            .collect();
        let in_clause = placeholders.join(", ");

        let query = format!(
            r#"
            SELECT id, created_at, updated_at, bangumi_id, rss_id, info_hash,
                   torrent_url, episode_number, subtitle_group, subtitle_language, resolution
            FROM torrent
            WHERE bangumi_id = $1 AND episode_number IN ({})
            "#,
            in_clause
        );

        let mut query_builder = sqlx::query_as::<_, Torrent>(&query)
            .bind(bangumi_id);

        for ep in episodes {
            query_builder = query_builder.bind(*ep);
        }

        let torrents: Vec<Torrent> = query_builder.fetch_all(pool).await?;

        // 按集数分组
        let mut result: HashMap<i32, Vec<Torrent>> = HashMap::new();
        for torrent in torrents {
            if let Some(ep) = torrent.episode_number {
                result.entry(ep).or_default().push(torrent);
            }
        }

        Ok(result)
    }
}
```

#### 性能对比

| 场景 | 优化前 | 优化后 |
|------|--------|--------|
| RSS 返回 50 items，48 个已存在 | 加载所有种子 + 50 次 HashSet 查找 | 1 次批量查询 + 2 个 item 详细处理 |
| 新番剧首次添加 RSS | 加载 0 个种子 | 1 次批量查询（返回空） |
| 大型番剧（100+ 集） | 加载 100+ 种子记录 | 只加载相关集数的种子 |

#### 优化效果

- **减少数据库负载**：从加载所有种子变为批量 IN 查询
- **减少内存占用**：只加载需要处理的种子信息
- **保持洗版能力**：对于新 item，仍会加载相关集数的现有种子进行优先级比较

---

### 八、文件变更清单

#### 新增文件
- `crates/server/src/priority.rs` - 优先级计算核心模块
- `web/src/features/settings/components/priority-section.tsx` - 前端优先级配置 UI

#### 修改文件
- `crates/server/src/models/settings.rs` - 添加优先级配置
- `crates/server/src/models/torrent.rs` - 添加解析信息字段
- `crates/server/src/models.rs` - 导出新类型
- `crates/server/src/services/rss_processing.rs` - 重构洗版逻辑
- `crates/server/src/services.rs` - 导出 priority 模块
- `crates/server/src/repositories/torrent.rs` - 支持新字段
- `crates/server/src/lib.rs` - 注册 priority 模块
- `migrations/001_init.sql` - 数据库模式变更
- `web/src/features/settings/schema.ts` - 添加优先级 schema
- `web/src/features/settings/hooks/use-settings-form.ts` - 处理优先级数据
- `web/src/features/settings/page.tsx` - 集成优先级配置
- `web/src/features/settings/components/settings-sidebar.tsx` - 添加侧边栏入口
- `web/src/features/settings/components/index.ts` - 导出组件
