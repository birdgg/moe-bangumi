# Downloader 模块架构文档

## 1. 概述

Downloader 模块负责与下载器（如 qBittorrent）交互，提供统一的下载任务管理接口。

### 设计目标

- **解耦**：业务逻辑与具体下载器实现分离
- **可靠性**：自动处理认证失效和重试
- **简洁性**：调用方无需关心底层通信细节
- **扩展性**：支持多种下载器后端（目前支持 qBittorrent）

## 2. 架构层次

```
┌─────────────────────────────────────────────────────────┐
│                    高层服务                              │
│  ┌─────────────────────┐  ┌─────────────────────────┐   │
│  │  TorrentCoordinator │  │     WashingService      │   │
│  │   (DB + 下载协调)    │  │    (洗版逻辑)           │   │
│  └──────────┬──────────┘  └───────────┬─────────────┘   │
└─────────────┼─────────────────────────┼─────────────────┘
              │                         │
              ▼                         ▼
┌─────────────────────────────────────────────────────────┐
│                  DownloaderService                       │
│              (DownloaderHandle 类型别名)                 │
│                                                          │
│  提供统一 API：add_task, delete_task, get_tasks 等       │
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
| 高层服务 | TorrentCoordinator | 协调数据库操作与下载任务 |
| 高层服务 | WashingService | 处理洗版（优先级替换）逻辑 |
| 服务层 | DownloaderService | 对外统一接口，隐藏 Actor 细节 |
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
  │                         │  send(AddTask)           │
  │                         │ ────────────────────────>│
  │                         │                          │ 执行操作
  │                         │                          │
  │  (无需等待)              │                          │
```

### 消息类型

Actor 接收以下消息类型：

| 消息 | 用途 | 是否等待响应 |
|------|------|-------------|
| `AddTask` | 添加下载任务 | 否 |
| `DeleteTask` | 删除下载任务 | 否 |
| `GetTasks` | 查询任务列表 | 是 |
| `GetTaskFiles` | 查询任务文件 | 是 |
| `IsAvailable` | 检查下载器状态 | 是 |
| `InvalidateClient` | 使客户端失效 | 否 |

### 重试机制

Actor 内部处理认证失效：

1. 执行操作
2. 如果认证失败，重新初始化客户端
3. 重试操作（最多 1 次）
4. 返回结果或错误

## 4. Fire-and-forget 操作

部分操作设计为「发射后不管」模式，调用方无需等待结果。

### Fire-and-forget 的操作

| 方法 | 原因 |
|------|------|
| `add_task()` | 下载任务入队即可，无需等待下载器响应 |
| `delete_task()` | 删除操作可后台执行，不阻塞业务流程 |

### 需要等待的操作

| 方法 | 原因 |
|------|------|
| `get_tasks()` | 需要返回任务列表数据 |
| `get_task_files()` | 需要返回文件列表数据 |
| `is_available()` | 需要返回状态检查结果 |

### 实现方式

Fire-and-forget 操作通过 `tokio::spawn` 发送消息，不等待响应：

```
fn add_task(&self, options: AddTaskOptions) {
    let sender = self.sender.clone();
    tokio::spawn(async move {
        let _ = sender.send(AddTask { options }).await;
    });
}
```

## 5. 服务依赖关系

### 依赖图

```
RssProcessingService ──────┐
                           │
                           ▼
                    TorrentCoordinator ────> DownloaderService
                           │
                           ▼
                      SqlitePool


WashingService ────────────────────────────> DownloaderService
      │
      ▼
 SqlitePool


RenameService ─────────────────────────────> DownloaderService
```

### 服务职责

| 服务 | 职责 | 使用的 Downloader 方法 |
|------|------|----------------------|
| TorrentCoordinator | 创建 DB 记录 + 添加下载 | `add_task()` |
| TorrentCoordinator | 删除 DB 记录 + 删除下载 | `delete_task()` |
| WashingService | 洗版：删旧任务 + 添新任务 | `delete_task()`, `add_task()` |
| RenameService | 查询任务状态和文件 | `get_tasks()`, `get_task_files()` |
| RenameService | 重命名文件 | `rename_file()` |

### TorrentCoordinator 的作用

TorrentCoordinator 封装了「数据库 + 下载器」的协调逻辑：

- `queue_download()`: 创建 torrent 记录，然后添加下载任务
- `delete_torrent()`: 删除 torrent 记录，然后删除下载任务

### WashingService 的作用

WashingService 处理洗版场景的完整流程：

- `wash_episode()`: 在事务中删除旧记录、创建新记录，然后删除旧下载、添加新下载

调用方只需调用一个方法即可完成洗版。
