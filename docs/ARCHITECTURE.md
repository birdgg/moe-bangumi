# moe-bangumi 架构文档

本项目采用 **Three Layer Cake** 架构模式，结合 **Effectful** 实现类型安全的副作用管理。

## 架构概览

```
┌─────────────────────────────────────────────────────────────┐
│                        App Layer                            │
│  Moe.App.*  (运行时环境、Effect 组合、程序入口)              │
├─────────────────────────────────────────────────────────────┤
│                      Adapter Layer                          │
│  Moe.Adapter.*  (Effect 解释器，run* 前缀)                  │
├─────────────────────────────────────────────────────────────┤
│                       Infra Layer                           │
│  Moe.Infra.*  (三方 API 封装，纯 IO)                        │
├─────────────────────────────────────────────────────────────┤
│                      Effect Layer                           │
│  Moe.Effect.*  (Effect 接口定义)                            │
├─────────────────────────────────────────────────────────────┤
│                      Domain Layer                           │
│  Moe.Domain.*  (纯业务逻辑、领域模型、解析器)                │
└─────────────────────────────────────────────────────────────┘
```

## 目录结构

```
src/Moe/
├── Domain/                          # Layer 3: 纯业务逻辑（无 Effect）
│   ├── Bangumi/
│   │   └── Types.hs                 # 番剧领域模型
│   ├── File/
│   │   ├── Types.hs                 # 文件类型定义
│   │   ├── Naming.hs                # 文件命名规则
│   │   └── Parser.hs                # 标题解析器 API
│   └── Internal/
│       └── File/                    # 内部实现
│
├── Effect/                          # Layer 2: Effect 定义
│   ├── Bangumi.hs                   # BangumiQuery, BangumiUpdate
│   ├── BangumiData.hs               # BangumiData
│   └── Metadata.hs                  # Metadata
│
├── Adapter/                         # Layer 1: Effect 实现
│   ├── Database/
│   │   ├── Bangumi.hs               # runBangumiQuerySQLite, runBangumiUpdateSQLite
│   │   └── Orphans.hs               # 序列化实例
│   └── Http/
│       ├── BangumiData.hs           # runBangumiDataHttp
│       └── Metadata.hs              # runMetadataHttp
│
├── Infra/                           # 三方 API 封装
│   └── BangumiData/
│       ├── Types.hs                 # API 响应类型
│       └── Client.hs                # HTTP 客户端
│
├── App/                             # 应用层
│   ├── Env.hs                       # 运行时环境配置
│   ├── Error.hs                     # 应用级错误类型
│   └── Monad.hs                     # MoeM 类型别名和 runMoe
│
└── Web/                             # Web 服务
    ├── Server.hs
    └── API/
```

## 层级职责

### Layer 3: Domain (`Moe.Domain.*`)

**纯函数层，无任何副作用**

| 模块 | 职责 |
|------|------|
| `Moe.Domain.Bangumi.Types` | 番剧领域模型，纯数据类型 |
| `Moe.Domain.File.Types` | 文件、集数、字幕等类型定义 |
| `Moe.Domain.File.Naming` | 文件命名规则 |
| `Moe.Domain.File.Parser` | 标题解析器公开 API |
| `Moe.Domain.Internal.*` | 内部实现细节 |

**设计原则**:
- 所有函数必须是纯函数
- 不导入任何 IO 或 Effect 相关模块
- 不包含序列化实例 (FromRow/ToRow)

```haskell
-- 纯数据类型示例
data Bangumi = Bangumi
  { id :: Maybe BangumiId,
    name :: Text,
    year :: Maybe Word16,
    ...
  }

-- 纯函数示例
generateFullPath :: BangumiFile -> FilePath
```

### Layer 2: Effect (`Moe.Effect.*`)

**Effect 接口定义层**

| 模块 | 职责 |
|------|------|
| `Moe.Effect.Bangumi` | BangumiQuery 和 BangumiUpdate Effect |
| `Moe.Effect.BangumiData` | BangumiData Effect |
| `Moe.Effect.Metadata` | Metadata Effect（多源搜索） |

**设计原则**:
- 使用 Effectful 的 Dynamic dispatch
- 只定义接口，不实现具体逻辑
- 只依赖 Domain 层的类型
- 使用 `effectful-th` 生成 send 函数

```haskell
data BangumiQuery :: Effect where
  GetBangumi :: Types.BangumiId -> BangumiQuery m (Maybe Types.Bangumi)
  ListBangumi :: BangumiQuery m [Types.Bangumi]

makeEffect ''BangumiQuery
```

### Layer 1: Adapter (`Moe.Adapter.*`)

**Effect 解释器**

| 模块 | 职责 |
|------|------|
| `Moe.Adapter.Database.Bangumi` | SQLite 数据库实现 |
| `Moe.Adapter.Http.BangumiData` | BangumiData HTTP 实现 |
| `Moe.Adapter.Http.Metadata` | Metadata HTTP 实现 |

**设计原则**:
- 实现 Effect 的具体解释器
- 函数使用 `run*` 前缀
- 可替换实现 (测试时用 mock)
- 调用 Infra 层的 API

```haskell
runBangumiQuerySQLite
  :: (SQLite :> es)
  => Eff (BangumiQuery : es) a
  -> Eff es a
runBangumiQuerySQLite = interpret $ \_ -> \case
  GetBangumi bid -> ...
  ListBangumi -> ...
```

### Infra (`Moe.Infra.*`)

**三方 API 封装层（纯 IO）**

| 模块 | 职责 |
|------|------|
| `Moe.Infra.BangumiData.Types` | API 响应类型、转换函数 |
| `Moe.Infra.BangumiData.Client` | HTTP 客户端封装 |

**设计原则**:
- 封装三方库的原始 API
- 纯 IO 函数，不涉及 Effect 系统
- 定义 API 相关的类型

```haskell
fetchByMonths :: Word16 -> [Int] -> IO (Either String [BangumiDataItem])
```

### Application (`Moe.App.*`)

**应用程序入口和配置**

| 模块 | 职责 |
|------|------|
| `Moe.App.Env` | 运行时环境配置 |
| `Moe.App.Error` | 应用级错误类型 |
| `Moe.App.Monad` | MoeM 类型别名和 runMoe |

```haskell
type MoeEffects =
  '[ BangumiQuery,
     BangumiUpdate,
     BangumiData,
     Metadata,
     SQLite,
     Error MoeError,
     IOE
   ]

type MoeM a = Eff MoeEffects a

runMoe :: MoeEnv -> MoeM a -> IO (Either MoeError a)
```

## 依赖规则

```
Domain  ←─────  Effect  ←─────  Adapter
                  ↑                ↓
                  └──────── Infra ─┘
                                ↑
              App ──────────────┘
```

| 层级 | 可依赖 | 禁止依赖 |
|------|--------|----------|
| Domain | 无 | Effect, Adapter, Infra, App |
| Effect | Domain, Infra(Types) | Adapter, App |
| Infra | Domain | Effect, Adapter, App |
| Adapter | Domain, Effect, Infra | App |
| App | Domain, Effect, Adapter, Infra | - |

## 模块导出策略

### 公开模块 (exposed-modules)

```
Moe.Domain.Bangumi.Types    -- 领域模型
Moe.Domain.File.Types       -- 文件类型
Moe.Domain.File.Naming      -- 文件命名 API
Moe.Domain.File.Parser      -- 解析器 API
Moe.Effect.Bangumi          -- Effect 接口
Moe.Effect.BangumiData      -- Effect 接口
Moe.Effect.Metadata         -- Effect 接口
Moe.Adapter.Database.Bangumi -- 数据库适配器
Moe.Adapter.Http.BangumiData -- HTTP 适配器
Moe.Adapter.Http.Metadata    -- HTTP 适配器
Moe.Infra.BangumiData.Types  -- API 类型
Moe.Infra.BangumiData.Client -- API 客户端
Moe.App.Env                  -- 环境配置
Moe.App.Error                -- 错误类型
Moe.App.Monad                -- App monad
```

### 内部模块 (other-modules)

```
Moe.Domain.Internal.*
Moe.Adapter.Database.Orphans
```

## Effect 组合示例

### 使用单个 Effect

```haskell
import Moe.Effect.Bangumi

getAllBangumi :: (BangumiQuery :> es) => Eff es [Types.Bangumi]
getAllBangumi = listBangumi
```

### 组合多个 Effect

```haskell
import Moe.Effect.Bangumi
import Moe.Effect.Metadata

searchAndCreate
  :: (BangumiQuery :> es, Metadata :> es)
  => Text
  -> Eff es [Types.Bangumi]
searchAndCreate keyword = do
  results <- searchBgmtv keyword Nothing
  -- ...
```

### 运行 App

```haskell
import Moe.App.Env (defaultMoeEnv)
import Moe.App.Monad (runMoe)

main :: IO ()
main = do
  result <- runMoe defaultMoeEnv $ do
    items <- listBangumi
    pure items
  case result of
    Left err -> print err
    Right items -> print items
```

## 测试策略

### Domain 层测试

纯函数，直接测试：

```haskell
testCase "generateFullPath" $ do
  let file = BangumiFile meta content fileType
  generateFullPath file @?= "expected/path"
```

### Effect 层测试

使用 mock 解释器：

```haskell
runBangumiQueryMock :: Eff (BangumiQuery : es) a -> Eff es a
runBangumiQueryMock = interpret $ \_ -> \case
  GetBangumi _ -> pure $ Just mockBangumi
  ListBangumi -> pure [mockBangumi]
```

### 集成测试

使用内存数据库：

```haskell
testWithDb :: MoeM a -> IO a
testWithDb action = do
  let env = defaultMoeEnv { databasePath = ":memory:" }
  result <- runMoe env action
  either (error . show) pure result
```

## 扩展指南

### 添加新 Effect

1. 在 `Moe.Effect/` 创建 Effect 定义
2. 在 `Moe.Adapter/` 创建解释器（使用 `run*` 前缀）
3. 在 `Moe.App.Monad` 添加到 MoeEffects
4. 组合解释器到 runMoe

### 添加新领域模型

1. 在 `Moe.Domain/` 创建纯数据类型
2. 在 `Moe.Adapter.Database.Orphans` 添加序列化实例
3. 如需持久化，创建对应 Effect

### 添加新三方 API 集成

1. 在 `Moe.Infra/` 创建 API 封装
2. 定义 API 类型和客户端函数
3. 在 `Moe.Adapter/` 创建 Effect 解释器

## 参考资源

- [Three Layer Haskell Cake](https://www.parsonsmatt.org/2018/03/22/three_layer_haskell_cake.html)
- [Effectful Documentation](https://hackage.haskell.org/package/effectful)
- [Holmusk/three-layer](https://github.com/Holmusk/three-layer)
