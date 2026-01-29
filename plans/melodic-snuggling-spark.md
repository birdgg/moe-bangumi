# Three Layer Cake + Effectful 风格目录结构设计

## 需求重述

为 moe-bangumi 项目设计符合 Haskell 和 Effectful 风格的目录结构，参考 Three Layer Cake 架构。

## 架构概述

Three Layer Cake 架构将代码分为三层：

1. **Layer 3 (Core/Domain)** - 纯业务逻辑，零副作用
2. **Layer 2 (Effects/Services)** - Effect 定义和领域服务接口
3. **Layer 1 (App/Infrastructure)** - 运行时、Effect 解释器、基础设施

Effectful 风格的关键特点：
- 使用 `Eff` monad 而非 mtl transformer stack
- Effect 定义与实现分离
- 静态/动态 dispatch 支持

## 推荐目录结构

```
src/
├── Moe/
│   ├── Core/                       # Layer 3: 纯业务逻辑
│   │   ├── Bangumi/                # 番剧领域
│   │   │   ├── File.hs             # 公开 API (重导出)
│   │   │   ├── Parser.hs           # 解析器公开 API
│   │   │   ├── Internal/           # 内部实现
│   │   │   │   ├── BangumiFile.hs
│   │   │   │   ├── Content.hs
│   │   │   │   ├── Extension.hs
│   │   │   │   ├── Group.hs
│   │   │   │   ├── Naming.hs
│   │   │   │   └── Subtitle.hs
│   │   │   └── Parser/
│   │   │       ├── Types.hs
│   │   │       ├── TitleParser.hs
│   │   │       └── Internal/
│   │   │           ├── Bracket.hs
│   │   │           └── EpisodePattern.hs
│   │   │
│   │   ├── Model/                  # 领域模型 (纯数据类型)
│   │   │   ├── Bangumi.hs          # 番剧实体
│   │   │   ├── Subscription.hs     # 订阅实体 (未来)
│   │   │   └── Internal/
│   │   │       └── Types.hs        # TmdbId, BgmtvId 等
│   │   │
│   │   └── Types.hs                # 共享纯类型定义
│   │
│   ├── Effect/                     # Layer 2: Effect 定义
│   │   ├── Bangumi.hs              # Bangumi Effect (CRUD 操作接口)
│   │   ├── Subscription.hs         # Subscription Effect
│   │   ├── FileSystem.hs           # 文件系统 Effect
│   │   ├── Http.hs                 # HTTP 请求 Effect
│   │   ├── Log.hs                  # 日志 Effect
│   │   └── Time.hs                 # 时间 Effect
│   │
│   ├── Service/                    # Layer 2: 业务服务 (组合 Effects)
│   │   ├── BangumiService.hs       # 番剧管理服务
│   │   ├── SubscriptionService.hs  # 订阅服务
│   │   ├── FileOrganizer.hs        # 文件整理服务
│   │   └── RssPoller.hs            # RSS 轮询服务 (未来)
│   │
│   ├── Infra/                      # Layer 1: 基础设施实现
│   │   ├── Effect/                 # Effect 解释器
│   │   │   ├── Bangumi.hs          # Bangumi Effect -> SQLite
│   │   │   ├── Subscription.hs
│   │   │   ├── FileSystem.hs       # FileSystem Effect -> IO
│   │   │   ├── Http.hs             # Http Effect -> http-client
│   │   │   ├── Log.hs              # Log Effect -> co-log/stdout
│   │   │   └── Time.hs
│   │   │
│   │   ├── Persistence/            # 数据库相关
│   │   │   ├── Schema.hs           # DB 序列化实例
│   │   │   ├── Bangumi.hs          # Bangumi 表操作
│   │   │   └── Subscription.hs
│   │   │
│   │   └── External/               # 外部服务客户端
│   │       ├── Tmdb.hs             # TMDB API
│   │       ├── BangumiTv.hs        # Bangumi.tv API
│   │       └── Mikan.hs            # 蜜柑计划 RSS
│   │
│   └── App/                        # Layer 1: 应用入口
│       ├── Env.hs                  # 运行时环境 (Config, 连接池等)
│       ├── Error.hs                # 应用级错误类型
│       ├── Monad.hs                # App monad 定义
│       └── Run.hs                  # Effect stack 运行器
│
app/
│   └── Main.hs                     # 可执行入口
│
test/
│   ├── Main.hs                     # 测试入口
│   ├── Core/                       # 纯逻辑测试
│   │   ├── BangumiSpec.hs
│   │   └── ParserSpec.hs
│   ├── Service/                    # 服务测试 (mock effects)
│   │   └── BangumiServiceSpec.hs
│   └── Integration/                # 集成测试
│       └── PersistenceSpec.hs
```

## 层级职责详解

### Layer 3: `Moe.Core.*`

**特点**: 完全纯函数，无任何 Effect 约束

```haskell
-- Moe.Core.Bangumi.Internal.Naming
generatePath :: BangumiFile -> FilePath  -- 纯函数，无 IO
```

**包含**:
- 领域模型数据类型
- 业务规则和验证逻辑
- 解析器和格式化器
- 工具函数

### Layer 2: `Moe.Effect.*` & `Moe.Service.*`

**Effect 定义** (接口层):

```haskell
-- Moe.Effect.Bangumi
data Bangumi :: Effect where
  GetBangumi :: BangumiId -> Bangumi m (Maybe Bangumi)
  CreateBangumi :: Bangumi -> Bangumi m BangumiId
  UpdateBangumi :: Bangumi -> Bangumi m ()
  DeleteBangumi :: BangumiId -> Bangumi m ()

type instance DispatchOf Bangumi = Dynamic

getBangumi :: Bangumi :> es => BangumiId -> Eff es (Maybe Bangumi)
createBangumi :: Bangumi :> es => Bangumi -> Eff es BangumiId
```

**Service 定义** (组合 Effects):

```haskell
-- Moe.Service.BangumiService
organizeFile
  :: (Bangumi :> es, FileSystem :> es, Log :> es)
  => SubscriptionItem
  -> Eff es FilePath
```

### Layer 1: `Moe.Infra.*` & `Moe.App.*`

**Effect 解释器**:

```haskell
-- Moe.Infra.Effect.Bangumi
runBangumiSqlite
  :: (IOE :> es)
  => Connection
  -> Eff (Bangumi : es) a
  -> Eff es a
```

**App Monad**:

```haskell
-- Moe.App.Monad
type AppEffects =
  '[ Bangumi
   , Subscription
   , FileSystem
   , Http
   , Log
   , Time
   , Error AppError
   , IOE
   ]

type App a = Eff AppEffects a

runApp :: AppEnv -> App a -> IO (Either AppError a)
```

## 迁移步骤

### 阶段 1: 重命名现有模块

| 原路径 | 新路径 |
|--------|--------|
| `Moe.Bangumi.*` | `Moe.Core.Bangumi.*` |
| `Moe.Model.*` | `Moe.Core.Model.*` |

### 阶段 2: 创建 Effect 层

1. 定义 `Moe.Effect.Bangumi` Effect
2. 将 `Moe.Model.Bangumi` 的 FromRow/ToRow 移至 `Moe.Infra.Persistence.Schema`
3. 保持 `Moe.Core.Model.Bangumi` 为纯数据类型

### 阶段 3: 创建 Infrastructure 层

1. 实现 `Moe.Infra.Effect.Bangumi` (SQLite 解释器)
2. 创建 `Moe.App.Env` 和 `Moe.App.Monad`

### 阶段 4: 创建 Service 层

根据业务需求逐步添加服务模块。

## 关键设计决策

### 1. Effect 粒度

**推荐**: 按领域概念定义 Effect，而非按技术能力

```haskell
-- 好: 领域导向
data Bangumi :: Effect where ...
data Subscription :: Effect where ...

-- 避免: 技术导向
data Database :: Effect where ...  -- 太宽泛
```

### 2. 纯模型 vs 持久化模型

**推荐**: Core 层的模型不包含序列化实例

```haskell
-- Moe.Core.Model.Bangumi (纯数据)
data Bangumi = Bangumi { ... }

-- Moe.Infra.Persistence.Schema (序列化)
instance FromRow Bangumi where ...
```

如果字段与数据库完全一致，可使用 orphan instance。若差异较大，考虑分离 `BangumiEntity` 和 `BangumiRow`。

### 3. Error 处理

使用 Effectful 的 `Error` effect：

```haskell
-- Moe.App.Error
data AppError
  = DatabaseError Text
  | NotFound Text
  | ValidationError Text
  | ExternalApiError Text
```

## Cabal 模块结构

```cabal
library
  exposed-modules:
    -- Core (纯逻辑)
    Moe.Core.Bangumi.File
    Moe.Core.Bangumi.Parser
    Moe.Core.Model.Bangumi
    Moe.Core.Types

    -- Effects (接口)
    Moe.Effect.Bangumi
    Moe.Effect.Log

    -- Services (业务组合)
    Moe.Service.BangumiService

    -- App (入口)
    Moe.App.Env
    Moe.App.Monad
    Moe.App.Run

  other-modules:
    -- Core Internal
    Moe.Core.Bangumi.Internal.*
    Moe.Core.Model.Internal.*

    -- Infrastructure (实现细节)
    Moe.Infra.Effect.Bangumi
    Moe.Infra.Persistence.Schema
```

## 验证方式

1. `cabal build` - 确保编译通过
2. `cabal test` - 运行所有现有测试
3. 检查模块导入，确保:
   - `Moe.Core.*` 不导入 `Moe.Infra.*` 或 `Moe.App.*`
   - `Moe.Effect.*` 只导入 `Moe.Core.*`
   - `Moe.Service.*` 只导入 `Moe.Core.*` 和 `Moe.Effect.*`

## 参考资源

- [Three Layer Haskell Cake](https://www.parsonsmatt.org/2018/03/22/three_layer_haskell_cake.html)
- [Holmusk/three-layer](https://github.com/Holmusk/three-layer)
- [kowainik/cake-slayer](https://github.com/kowainik/cake-slayer)
- [effectful GitHub](https://github.com/haskell-effectful/effectful)
