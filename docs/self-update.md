# Self-Update

moe-bangumi 支持在运行时自动更新可执行文件，无需手动替换。整个流程依赖 Supervisor 进程监控 + GitHub Releases API + 原子文件替换三个核心机制协同工作。

## 架构概览

```
┌─────────────────────────────────────────────────────┐
│  main (无 --supervised 参数)                         │
│  └─ Supervisor 进程                                  │
│     └─ fork 子进程: moe-bangumi --supervised         │
│        └─ 应用主进程 (Web Server + Workers)           │
│           ├─ GET  /api/about   → 检查更新            │
│           └─ POST /api/update  → 执行更新            │
│                                                      │
│  更新流程:                                           │
│  子进程 exit 0 → Supervisor 检测 → 1s 后重启子进程    │
│  (新二进制文件已就位，重启即生效)                       │
└─────────────────────────────────────────────────────┘
```

## Supervisor 机制

入口在 `app/Main.hs`。程序启动时根据是否有 `--supervised` 参数分为两条路径：

- **无参数启动**: 进入 Supervisor 模式，fork 一个带 `--supervised` 的子进程
- **`--supervised` 启动**: 运行实际的应用逻辑（Web Server、RSS Worker 等）

Supervisor 的核心逻辑在 `app/Supervisor.hs`：

1. 通过 `getExecutablePath` 获取当前可执行文件路径
2. 使用 `createProcess` 启动子进程，传入 `--supervised` 参数
3. `waitForProcess` 等待子进程退出
4. 根据退出码决定行为：
   - **Exit 0**: 等待 1 秒后重启子进程（更新成功的约定退出码）
   - **非零退出码**: Supervisor 也退出，不再重启
5. 信号处理：捕获 `SIGTERM`/`SIGINT`，优雅终止子进程

## 检查更新

### API

`GET /api/about` → 返回 `AboutResponse`

### 流程

1. 从 `moe-bangumi.cabal` 中的 `version` 字段获取当前版本（编译时嵌入）
2. 调用 GitHub Releases API: `GET https://api.github.com/repos/birdgg/moe-bangumi/releases/latest`
3. 解析响应，提取 `tag_name`（去掉 `v` 前缀）作为最新版本
4. 比较版本号（语义化版本逐段比较）
5. 返回 `AboutInfo`，包含：
   - `needUpdate`: 远端版本是否严格大于当前版本
   - `autoUpdate`: 是否可以自动更新（仅当 major.minor 相同时允许，即只允许 patch 级别自动更新）
   - `downloadUrl`: 对应平台 tarball 的下载地址
   - `checksumUrl`: `checksums.txt` 的下载地址

### 缓存

版本信息缓存在 `TVar` 中，TTL 为 20 分钟（`cacheTTL = 1200` 秒），避免频繁请求 GitHub API。

### 平台检测

通过 `System.Info` 检测当前运行环境：

| 字段 | 值 |
|------|----|
| Platform | `linux` / `darwin` |
| Arch | `amd64` / `arm64` |

tarball 命名格式: `moe-bangumi-{platform}-{arch}.tar.gz`

## 执行更新

### API

`POST /api/update` → 触发自更新

### 流程

```
检查更新 → 下载 tarball → 下载并校验 checksum
→ 解压二进制 → 设置执行权限 → 原子替换文件 → exit 0
→ Supervisor 检测到 exit 0 → 重启子进程（加载新二进制）
```

详细步骤：

1. **检查更新**: 调用 `checkForUpdate`，如果 `needUpdate = False` 则直接返回
2. **准备临时目录**: 在可执行文件同级目录创建 `.update-tmp/`
3. **下载 tarball**: 使用流式下载（`withResponse` + 分块读取），避免内存峰值
4. **校验完整性**: 下载 `checksums.txt`，解析出目标文件的 SHA256 哈希，与下载文件的实际哈希比对
5. **解压**: 调用 `tar xzf` 解压二进制文件
6. **设置权限**: `setOwnerExecutable True`
7. **原子替换**: 使用 `safeReplaceFile` 安全替换当前二进制（见下文）
8. **清理**: 删除 `.update-tmp/` 临时目录
9. **退出**: 调用 `exitSuccess`（exit code 0），触发 Supervisor 重启

### 原子文件替换（safeReplaceFile）

为了保证替换过程的安全性，采用备份 + 回滚机制：

```
1. target → target.backup      (备份当前文件)
2. source → target.new          (新文件移到目标位置旁)
3. target.new → target          (原子替换)
4. 删除 target.backup           (清理备份)
```

如果步骤 2-3 失败，自动回滚：将 `target.backup` 恢复为 `target`。

`atomicRenameOrCopy` 优先使用 `renameFile`（同文件系统下原子操作），失败时回退到 `copyFileWithMetadata` + `removeFile`（跨文件系统兼容）。

## 错误处理

所有更新错误统一为 `UpdateClientError` 类型：

| 错误类型 | 场景 |
|----------|------|
| `UpdateNetworkError` | GitHub API 请求失败、下载失败 |
| `UpdateChecksumMismatch` | SHA256 校验不通过 |
| `UpdateUnsupportedPlatform` | 不支持的操作系统或架构 |
| `UpdateExtractionFailed` | tar 解压失败 |
| `UpdateFileError` | 文件替换失败（含回滚状态） |

错误通过 `AppError (UpdateError ...)` 传播，API 层返回 HTTP 500。

## Effect 架构

遵循 effectful 模式，更新功能抽象为 `Update` effect：

```
Effect 定义:  src/Moe/Infra/Update/Effect.hs
  ├─ CheckForUpdate :: Update m AboutInfo
  └─ PerformUpdate  :: Update m ()

类型定义:    src/Moe/Infra/Update/Types.hs
  ├─ AboutInfo, PlatformInfo, UpdateClientError
  └─ detectPlatform, isNewer, sameMajorMinor

解释器:      src/Moe/Infra/Update/Adapter.hs
  └─ runUpdateGitHub (GitHub Releases 实现)

模块入口:    src/Moe/Infra/Update.hs (re-export)
```

`runUpdateGitHub` 在 `Web.Server.naturalTransform` 中注入到 effect stack。

## CI/CD 与 Release

推送 `v*` 格式的 git tag 触发 `.github/workflows/release.yml`：

1. **构建前端**: bun install + bun run build
2. **构建二进制**: Docker 多阶段构建，支持 linux/amd64 和 linux/arm64
3. **打包 tarball**: `moe-bangumi-linux-{arch}.tar.gz`
4. **生成 checksums**: `sha256sum` 生成 `checksums.txt`
5. **创建 GitHub Release**: 上传 tarball + checksums.txt + changelog
6. **推送 Docker 镜像**: 多架构 manifest 推送到 ghcr.io

Release 产物即为自更新系统下载的目标文件。
