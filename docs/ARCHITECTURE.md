# moe-bangumi 架构文档

本项目采用 **Three Layer Cake** 架构模式，结合 **Effectful** 实现类型安全的副作用管理。

## 架构概览

```
┌─────────────────────────────────────────────────────────────┐
│                        App Layer                             │
│  Moe.App.*  (运行时环境、Effect 组合、程序入口)              │
├─────────────────────────────────────────────────────────────┤
│                      Infra Layer                             │
│  Moe.Infra.*  (Effect 解释器、数据库、外部服务)              │
├─────────────────────────────────────────────────────────────┤
│                     Effect Layer                             │
│  Moe.Effect.*  (Effect 接口定义)                            │
├─────────────────────────────────────────────────────────────┤
│                      Core Layer                              │
│  Moe.Core.*  (纯业务逻辑、领域模型、解析器)                  │
└─────────────────────────────────────────────────────────────┘
```

## 层级职责

### Layer 3: Core (`Moe.Core.*`)

**纯函数层，无任何副作用**

| 模块 | 职责 |
|------|------|
| `Moe.Core.Model.Bangumi` | 番剧领域模型，纯数据类型 |
| `Moe.Core.Model.Internal.Types` | 共享类型 (TmdbId, BgmtvId) |
| `Moe.Core.Bangumi.File` | 文件命名规则公开 API |
| `Moe.Core.Bangumi.Parser` | 标题解析器公开 API |
| `Moe.Core.Bangumi.Internal.*` | 内部实现细节 |

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
| `Moe.Effect.Bangumi` | Bangumi CRUD 操作 Effect |

**设计原则**:
- 使用 Effectful 的 Dynamic dispatch
- 只定义接口，不实现具体逻辑
- 只依赖 Core 层的类型

```haskell
data Bangumi :: Effect where
  GetBangumi :: BangumiId -> Bangumi m (Maybe Model.Bangumi)
  ListBangumi :: Bangumi m [Model.Bangumi]
  CreateBangumi :: Model.Bangumi -> Bangumi m BangumiId
  UpdateBangumi :: Model.Bangumi -> Bangumi m ()
  DeleteBangumi :: BangumiId -> Bangumi m ()

type instance DispatchOf Bangumi = Dynamic
```

### Layer 1: Infrastructure (`Moe.Infra.*`)

**Effect 解释器和外部系统集成**

| 模块 | 职责 |
|------|------|
| `Moe.Infra.Effect.Bangumi` | Bangumi Effect 的 SQLite 实现 |
| `Moe.Infra.Persistence.Schema` | 数据库序列化实例 |

**设计原则**:
- 实现 Effect 的具体解释器
- 处理所有 IO 操作
- 可替换实现 (测试时用 mock)

```haskell
runBangumiSQLite
  :: (SQLite :> es)
  => Eff (Bangumi : es) a
  -> Eff es a
runBangumiSQLite = interpret $ \_ -> \case
  GetBangumi bid -> ...
  CreateBangumi bangumi -> ...
```

### Layer 1: Application (`Moe.App.*`)

**应用程序入口和配置**

| 模块 | 职责 |
|------|------|
| `Moe.App.Env` | 运行时环境配置 |
| `Moe.App.Error` | 应用级错误类型 |
| `Moe.App.Monad` | App monad 类型别名 |
| `Moe.App.Run` | Effect stack 运行器 |

```haskell
type AppEffects =
  '[ Bangumi,
     SQLite,
     Error AppError,
     IOE
   ]

type App a = Eff AppEffects a

runApp :: AppEnv -> App a -> IO (Either AppError a)
```

## 依赖规则

```
Core  ←─────  Effect  ←─────  Infra
  ↑                             ↑
  └─────────────────────────────┘
                                ↑
              App ──────────────┘
```

| 层级 | 可依赖 | 禁止依赖 |
|------|--------|----------|
| Core | 无 | Effect, Infra, App |
| Effect | Core | Infra, App |
| Infra | Core, Effect | App |
| App | Core, Effect, Infra | - |

## 模块导出策略

### 公开模块 (exposed-modules)

```
Moe.Core.Bangumi.File      -- 文件命名 API
Moe.Core.Bangumi.Parser    -- 解析器 API
Moe.Core.Model.Bangumi     -- 领域模型
Moe.Effect.Bangumi         -- Effect 接口
Moe.App.Env                -- 环境配置
Moe.App.Error              -- 错误类型
Moe.App.Monad              -- App monad
Moe.App.Run                -- 运行器
```

### 内部模块 (other-modules)

```
Moe.Core.Bangumi.Internal.*
Moe.Core.Model.Internal.*
Moe.Infra.Effect.*
Moe.Infra.Persistence.*
```

## Effect 组合示例

### 使用单个 Effect

```haskell
import Moe.Effect.Bangumi

getAllBangumi :: (Bangumi :> es) => Eff es [Model.Bangumi]
getAllBangumi = listBangumi
```

### 组合多个 Effect

```haskell
import Moe.Effect.Bangumi
import Effectful.Sqlite (SQLite, withTransaction)

createWithTransaction
  :: (Bangumi :> es, SQLite :> es)
  => Model.Bangumi
  -> Eff es BangumiId
createWithTransaction bangumi =
  withTransaction $ createBangumi bangumi
```

### 运行 App

```haskell
import Moe.App.Env (defaultAppEnv)
import Moe.App.Run (runApp)

main :: IO ()
main = do
  result <- runApp defaultAppEnv $ do
    bid <- createBangumi newBangumi
    getBangumi bid
  case result of
    Left err -> print err
    Right bangumi -> print bangumi
```

## 测试策略

### Core 层测试

纯函数，直接测试：

```haskell
testCase "generateFullPath" $ do
  let file = BangumiFile meta content fileType
  generateFullPath file @?= "expected/path"
```

### Effect 层测试

使用 mock 解释器：

```haskell
runBangumiMock :: Eff (Bangumi : es) a -> Eff es a
runBangumiMock = interpret $ \_ -> \case
  GetBangumi _ -> pure $ Just mockBangumi
  ...
```

### 集成测试

使用内存数据库：

```haskell
testWithDb :: App a -> IO a
testWithDb action = do
  env <- AppEnv ":memory:"
  result <- runApp env action
  either (error . show) pure result
```

## 扩展指南

### 添加新 Effect

1. 在 `Moe.Effect/` 创建 Effect 定义
2. 在 `Moe.Infra.Effect/` 创建解释器
3. 在 `Moe.App.Monad` 添加到 AppEffects
4. 在 `Moe.App.Run` 组合解释器

### 添加新领域模型

1. 在 `Moe.Core.Model/` 创建纯数据类型
2. 在 `Moe.Infra.Persistence.Schema` 添加序列化实例
3. 如需持久化，创建对应 Effect

### 添加新业务逻辑

1. 纯逻辑放入 `Moe.Core.*`
2. 需要副作用的逻辑放入 `Moe.Service.*` (未来)

## 参考资源

- [Three Layer Haskell Cake](https://www.parsonsmatt.org/2018/03/22/three_layer_haskell_cake.html)
- [Effectful Documentation](https://hackage.haskell.org/package/effectful)
- [Holmusk/three-layer](https://github.com/Holmusk/three-layer)
