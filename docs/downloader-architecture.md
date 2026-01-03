# Downloader 模块架构文档

## 1. 概述

Downloader 模块负责与下载器（如 qBittorrent）交互，提供统一的下载任务管理接口。

### 设计目标

- **解耦**：业务逻辑与具体下载器实现分离
- **可靠性**：自动处理认证失效和重试
- **简洁性**：调用方无需关心底层通信细节
- **封装性**：Tag 管理等内部逻辑对外透明
- **扩展性**：支持多种下载器后端（目前支持 qBittorrent）

## 2. 架构层次

```
┌─────────────────────────────────────────────────────────┐
│                    高层服务                              │
│  ┌─────────────────────┐  ┌─────────────────────────┐   │
│  │ RssProcessingService│  │     WashingService      │   │
│  │   (RSS 处理)         │  │    (洗版逻辑)           │   │
│  └──────────┬──────────┘  └───────────┬─────────────┘   │
│             │                         │                  │
│  ┌──────────┴─────────────────────────┴─────────────┐   │
│  │              RenameService (文件重命名)            │   │
│  └──────────────────────┬───────────────────────────┘   │
└─────────────────────────┼───────────────────────────────┘
                          │
                          ▼
┌─────────────────────────────────────────────────────────┐
│                  DownloaderService                       │
│              (DownloaderHandle 类型别名)                 │
│                                                          │
│  提供统一 API：add_task, delete_task, get_tasks 等       │
│  内部管理 tag：自动添加 moe/rename，封装 tag 过滤        │
└──────────────────────────┬──────────────────────────────┘
                           │ mpsc channel
                           ▼
┌─────────────────────────────────────────────────────────┐
│                   DownloaderActor                        │
│                                                          │
│  - 接收消息并执行操作                                     │
│  - 管理客户端状态                                         │
│  - 处理认证失效和重试                                     │
└──────────────────────────┬──────────────────────────────┘
                           │
                           ▼
┌─────────────────────────────────────────────────────────┐
│                  DownloaderClient                        │
│                                                          │
│  具体下载器实现（qBittorrent API 调用）                   │
└─────────────────────────────────────────────────────────┘
```

### 层次说明

| 层次 | 组件 | 职责 |
|------|------|------|
| 高层服务 | RssProcessingService | 处理 RSS 订阅，协调数据库与下载任务 |
| 高层服务 | WashingService | 处理洗版（优先级替换）逻辑 |
| 高层服务 | RenameService | 重命名已完成任务的文件 |
| 服务层 | DownloaderService | 对外统一接口，隐藏 Actor 和 Tag 细节 |
| Actor 层 | DownloaderActor | 状态管理、消息处理、重试逻辑 |
| 客户端层 | DownloaderClient | 具体下载器 API 实现 |

## 3. Actor 模式

DownloaderService 采用 Actor 模式实现，将状态管理集中在单一 Actor 内。

### 消息流

```
调用方                    Handle                     Actor
  │                         │                          │
  │  add_task(options)      │                          │
  │ ───────────────────────>│                          │
  │                         │  send(AddTask, reply)    │
  │                         │ ────────────────────────>│
  │                         │                          │ 执行操作
  │                         │          Result          │
  │  Result<String>         │ <────────────────────────│
  │ <───────────────────────│                          │
```

### 消息类型

Actor 接收以下消息类型：

| 消息 | 用途 | 返回类型 |
|------|------|----------|
| `AddTask` | 添加下载任务 | `Result<String>` |
| `DeleteTask` | 删除下载任务 | `Result<()>` |
| `GetTasks` | 查询任务列表 | `Result<Vec<Task>>` |
| `GetTaskFiles` | 查询任务文件 | `Result<Vec<TaskFile>>` |
| `AddTags` | 添加标签 | `Result<()>` |
| `RemoveTags` | 移除标签 | `Result<()>` |
| `RenameFile` | 重命名文件 | `Result<()>` |
| `GetRenamePendingTasks` | 获取待重命名任务 | `Result<Vec<Task>>` |
| `CompleteRename` | 标记重命名完成 | `Result<()>` |
| `IsAvailable` | 检查下载器状态 | `bool` |
| `InvalidateClient` | 使客户端失效 | - |

### 重试机制

Actor 内部处理认证失效：

1. 执行操作
2. 如果认证失败，重新初始化客户端
3. 重试操作（最多 1 次）
4. 返回结果或错误

## 4. Tag 管理

Downloader 内部使用 tag 来跟踪任务状态，调用方无需关心具体 tag 值。

### 内部 Tag

| Tag | 用途 | 自动添加时机 |
|-----|------|-------------|
| `moe` | 标识由本应用添加的任务 | `add_task()` 时自动添加 |
| `rename` | 标识待重命名的任务 | `add_task()` 时自动添加 |

### 封装的 API

| 方法 | 功能 |
|------|------|
| `get_rename_pending_tasks()` | 获取已完成且待重命名的任务（内部过滤 rename tag） |
| `complete_rename(id)` | 标记任务重命名完成（内部移除 rename tag） |

调用方只需使用这些高层 API，无需了解 tag 细节。

## 5. 服务依赖关系

### 依赖图

```
RssProcessingService ─────┬────────────────> DownloaderService
                          │
                          ▼
                     SqlitePool


WashingService ───────────┬────────────────> DownloaderService
                          │
                          ▼
                     SqlitePool


RenameService ────────────────────────────> DownloaderService
```

### 服务职责

| 服务 | 职责 | 使用的 Downloader 方法 |
|------|------|----------------------|
| RssProcessingService | 创建 DB 记录 + 添加下载 | `add_task()` |
| WashingService | 洗版：删旧任务 + 添新任务 | `delete_task()`, `add_task()` |
| RenameService | 获取待重命名任务 | `get_rename_pending_tasks()` |
| RenameService | 查询任务文件 | `get_task_files()` |
| RenameService | 重命名文件 | `rename_file()` |
| RenameService | 标记重命名完成 | `complete_rename()` |

### RssProcessingService 的作用

RssProcessingService 封装了 RSS 处理的完整流程：

- 解析 RSS 条目，创建 torrent 记录
- 使用事务确保数据库与下载器操作一致性
- 调用 WashingService 处理优先级替换

### WashingService 的作用

WashingService 处理洗版场景的完整流程：

- `wash_episode()`: 在事务中删除旧记录、创建新记录，然后删除旧下载、添加新下载

调用方只需调用一个方法即可完成洗版。

## 6. 错误处理

所有 Downloader 操作都返回 `Result`，调用方可以：

1. 处理错误并回滚事务
2. 记录错误日志
3. 向用户报告失败

```rust
// 示例：事务中的错误处理
let mut tx = db.begin().await?;

// 创建数据库记录
TorrentRepository::create_with_executor(&mut *tx, torrent).await?;

// 添加下载任务，失败时事务自动回滚
if let Err(e) = downloader.add_task(options).await {
    tracing::error!("Failed to add download task: {}", e);
    return; // tx 被 drop，自动回滚
}

// 全部成功，提交事务
tx.commit().await?;
```
