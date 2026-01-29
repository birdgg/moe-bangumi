# 重构计划：参考 flora-server 项目结构

## 需求重述

将 moe-bangumi 项目的结构重构为类似 flora-server 的多库架构模式：
- 将单一主库拆分为多个独立子库
- 采用 Query/Types/Update 模式组织模型
- 保持 Three Layer Cake + Effectful 架构的优势
- 改进模块命名和组织结构

## 当前结构 vs 目标结构

### 当前结构
```
moe-bangumi/
├── src/
│   ├── bangumi-data/           # 独立库（✓ 保留）
│   └── moe-bangumi/            # 主库（需拆分）
│       ├── Moe/
│       │   ├── Core/           # 领域模型
│       │   ├── Effect/         # Effect 接口
│       │   └── Infra/          # Effect 实现
│       └── Web/                # Web 层
└── app/
```

### 目标结构
```
moe-bangumi/
├── src/
│   ├── core/                   # library moe-bangumi (核心库)
│   │   └── Moe/
│   │       ├── Environment/    # 环境配置
│   │       │   ├── Env.hs
│   │       │   └── Config.hs
│   │       ├── Model/          # 领域模型 (Query/Types/Update)
│   │       │   ├── Bangumi/
│   │       │   │   ├── Types.hs
│   │       │   │   ├── Query.hs
│   │       │   │   └── Update.hs
│   │       │   └── Subscription/
│   │       │       ├── Types.hs
│   │       │       ├── Query.hs
│   │       │       └── Update.hs
│   │       ├── Monad.hs        # MoeM 类型别名
│   │       └── Logging.hs
│   │
│   ├── file/                   # library moe-file (文件处理库)
│   │   └── Moe/
│   │       └── File/
│   │           ├── Naming.hs   # 文件命名规则
│   │           ├── Parser.hs   # 文件名解析
│   │           ├── Types.hs    # 文件类型定义
│   │           └── Internal/   # 内部模块
│   │
│   ├── web/                    # library moe-web (Web 层库)
│   │   └── MoeWeb/
│   │       ├── API/
│   │       │   ├── Routes.hs
│   │       │   ├── Server.hs
│   │       │   └── Errors.hs
│   │       ├── Server.hs
│   │       ├── Session.hs
│   │       └── Types.hs
│   │
│   └── bangumi-data/           # library bangumi-data (保留)
│
├── app/
│   ├── server/
│   │   └── Main.hs
│   └── cli/                    # 新增 CLI 工具
│       └── Main.hs
│
└── test/
```

## 实施阶段

### Phase 1: 创建目录结构
**任务：**
1. 创建 `src/core/` 目录
2. 创建 `src/file/` 目录
3. 创建 `src/web/` 目录
4. 创建 `app/server/` 和 `app/cli/` 目录

### Phase 2: 拆分核心库 (moe-bangumi → moe-core)
**任务：**
1. 将 `Moe.App.Env` 移动到 `Moe.Environment.Env`
2. 将 `Moe.App.Error` 移动到 `Moe.Environment.Config` 或单独模块
3. 将 `Moe.App.Monad` 简化为 `Moe.Monad`
4. 采用 Query/Types/Update 模式重组模型：
   - `Moe.Core.Model.Bangumi` → `Moe.Model.Bangumi.Types`
   - `Moe.Effect.Bangumi` (查询部分) → `Moe.Model.Bangumi.Query`
   - `Moe.Effect.Bangumi` (更新部分) → `Moe.Model.Bangumi.Update`
5. 将 Effect 实现合并到 Model 中（像 flora-server 那样）

### Phase 3: 拆分文件处理库 (moe-file)
**任务：**
1. 将 `Moe.Core.Bangumi.File` 移动到 `Moe.File.Naming`
2. 将 `Moe.Core.Bangumi.Parser` 移动到 `Moe.File.Parser`
3. 将内部模块移动到 `Moe.File.Internal/`
4. 创建 `Moe.File.Types` 整合类型定义

### Phase 4: 拆分 Web 层库 (moe-web)
**任务：**
1. 将 `Web.*` 重命名为 `MoeWeb.*`
2. 整理 API 模块结构：
   - `MoeWeb.API.Routes`
   - `MoeWeb.API.Server`
   - `MoeWeb.API.Errors`
3. 添加通用模块：
   - `MoeWeb.Common.Pagination`
   - `MoeWeb.Common.Guards`

### Phase 5: 更新 Cabal 配置
**任务：**
1. 更新 `moe-bangumi.cabal`：
   - 定义 `library moe-bangumi`（核心库）
   - 定义 `library moe-file`（文件处理库）
   - 定义 `library moe-web`（Web 层库）
   - 保留 `library bangumi-data`
2. 配置库间依赖关系：
   ```
   moe-file ← 无依赖（纯函数库）
   moe-bangumi ← moe-file, bangumi-data
   moe-web ← moe-bangumi, moe-file
   ```
3. 添加 `common-rts-options` 用于可执行文件

### Phase 6: 创建 CLI 可执行文件
**任务：**
1. 创建 `app/cli/Main.hs`
2. 添加基本 CLI 命令结构（使用 optparse-applicative）

### Phase 7: 更新测试
**任务：**
1. 更新测试导入路径
2. 确保所有测试通过

## 模块命名规范

### 遵循 flora-server 的命名模式

| 模式 | 说明 | 示例 |
|------|------|------|
| `Moe.Model.{Entity}.Types` | 实体类型定义 | `Moe.Model.Bangumi.Types` |
| `Moe.Model.{Entity}.Query` | SELECT 查询 | `Moe.Model.Bangumi.Query` |
| `Moe.Model.{Entity}.Update` | INSERT/UPDATE/DELETE | `Moe.Model.Bangumi.Update` |
| `Moe.Model.{Entity}.Guard` | 权限验证 | `Moe.Model.Bangumi.Guard` |
| `Moe.Environment.Env` | 环境配置 | - |
| `Moe.Environment.Config` | 配置解析 | - |
| `MoeWeb.API.Routes` | API 路由定义 | - |
| `MoeWeb.API.Server` | API 处理器 | - |

## 依赖关系图

```
                    ┌─────────────────┐
                    │  bangumi-data   │
                    │   (HTTP API)    │
                    └────────┬────────┘
                             │
┌─────────────┐              │
│  moe-file   │              │
│ (纯函数库)  │              │
└──────┬──────┘              │
       │                     │
       ▼                     ▼
┌─────────────────────────────────────┐
│          moe-bangumi (core)         │
│  - Environment (Env, Config)        │
│  - Model (Bangumi, Subscription)    │
│  - Monad (MoeM 类型别名)            │
└────────────────┬────────────────────┘
                 │
                 ▼
┌─────────────────────────────────────┐
│            moe-web                  │
│  - API (Routes, Server, Errors)     │
│  - Server (Warp 配置)               │
│  - Session, Types                   │
└─────────────────────────────────────┘
                 │
                 ▼
┌─────────────────────────────────────┐
│      Executables                    │
│  - moe-server (app/server)          │
│  - moe-cli (app/cli)                │
└─────────────────────────────────────┘
```

## 风险评估

| 风险 | 级别 | 缓解措施 |
|------|------|----------|
| 大量文件移动可能导致 git 历史丢失 | 中 | 使用 `git mv` 并分批提交 |
| 模块重命名可能破坏导入 | 中 | 逐步迁移，每步验证编译 |
| Effect 实现位置变更 | 低 | 保持接口兼容，只移动实现 |

## 预期收益

1. **更清晰的模块边界** - 每个库职责单一
2. **更好的编译性能** - 独立库可并行编译
3. **更易于测试** - 纯函数库可独立测试
4. **遵循社区最佳实践** - 与 flora-server 保持一致的架构模式

## 不变的部分

- GHC 版本和语言标准保持不变（GHC 9.12.2, GHC2024）
- Effectful 作为 Effect 系统不变
- Relude 作为 Prelude 替代不变
- Tasty 作为测试框架不变
- Servant + Warp 作为 Web 框架不变
