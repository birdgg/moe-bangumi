# Bangumi-Data 按季度获取实现计划

## 需求概述

从 [bangumi-data](https://github.com/bangumi-data/bangumi-data) 获取每季度番剧数据，支持按季度（如 2026Q1）筛选，并同步到本地数据库。

## 实现方案

### 1. Core 层 - 新增模块

#### 1.1 季度模型 `Moe.Core.Model.Season`

新建文件：`src/Moe/Core/Model/Season.hs`

```haskell
module Moe.Core.Model.Season
  ( Season (..),
    Quarter (..),
    seasonFromDate,
    seasonToText,
  )
where

data Quarter = Q1 | Q2 | Q3 | Q4
  deriving stock (Eq, Show, Ord, Enum, Bounded)

data Season = Season
  { year :: Word16,
    quarter :: Quarter
  }
  deriving stock (Eq, Show, Ord)

seasonFromMonth :: Word16 -> Int -> Season
seasonToText :: Season -> Text  -- "2026Q1"
```

#### 1.2 BangumiData 模型 `Moe.Core.Model.BangumiData`

新建文件：`src/Moe/Core/Model/BangumiData.hs`

```haskell
module Moe.Core.Model.BangumiData
  ( BangumiDataItem (..),
    BangumiDataSite (..),
    TitleTranslate (..),
    extractMikanId,
    extractBgmtvId,
    extractTmdbId,
    itemToSeason,
    toBangumi,
  )
where

data BangumiDataItem = BangumiDataItem
  { title :: Text,
    titleTranslate :: TitleTranslate,
    itemType :: Text,
    lang :: Text,
    begin :: Maybe Text,
    sites :: [BangumiDataSite]
  }

data BangumiDataSite = BangumiDataSite
  { site :: Text,
    siteId :: Text
  }

data TitleTranslate = TitleTranslate
  { zhHans :: [Text],
    zhHant :: [Text]
  }

-- 纯函数：提取各平台 ID
extractMikanId :: [BangumiDataSite] -> Maybe MikanId
extractBgmtvId :: [BangumiDataSite] -> Maybe BgmtvId
extractTmdbId :: [BangumiDataSite] -> Maybe TmdbId

-- 纯函数：从 begin 字段解析季度
itemToSeason :: BangumiDataItem -> Maybe Season

-- 纯函数：转换为 Bangumi 领域模型
toBangumi :: BangumiDataItem -> Bangumi
```

#### 1.3 更新 Bangumi 模型

修改文件：`src/Moe/Core/Model/Bangumi.hs`

```haskell
data Bangumi = Bangumi
  { id :: Maybe BangumiId,
    name :: Text,
    year :: Maybe Word16,
    season :: Maybe Season,      -- 新增
    mikanId :: Maybe MikanId,    -- 新增
    tmdbId :: Maybe TmdbId,
    bgmtvId :: Maybe BgmtvId,
    posterUrl :: Maybe Text,
    overview :: Maybe Text
  }
```

#### 1.4 更新 Internal Types

修改文件：`src/Moe/Core/Model/Internal/Types.hs`

```haskell
-- 新增
newtype MikanId = MikanId Word32
  deriving stock (Eq, Show)
  deriving newtype (Num)
```

### 2. Effect 层 - 新增 BangumiData Effect

新建文件：`src/Moe/Effect/BangumiData.hs`

```haskell
module Moe.Effect.BangumiData
  ( BangumiData (..),
    fetchBangumiDataBySeason,
  )
where

data BangumiData :: Effect where
  FetchBangumiDataBySeason :: Season -> BangumiData m [BangumiDataItem]

type instance DispatchOf BangumiData = Dynamic

fetchBangumiDataBySeason
  :: (HasCallStack, BangumiData :> es)
  => Season
  -> Eff es [BangumiDataItem]
```

### 3. Infra 层 - HTTP 实现

新建文件：`src/Moe/Infra/Effect/BangumiData.hs`

```haskell
module Moe.Infra.Effect.BangumiData
  ( runBangumiDataHttp,
  )
where

-- 数据源 URL
bangumiDataUrl :: Text
bangumiDataUrl = "https://raw.githubusercontent.com/bangumi-data/bangumi-data/master/dist/data.json"

runBangumiDataHttp
  :: (IOE :> es, Error AppError :> es)
  => Eff (BangumiData : es) a
  -> Eff es a
runBangumiDataHttp = interpret $ \_ -> \case
  FetchBangumiDataBySeason targetSeason -> do
    -- 1. HTTP GET 获取 JSON
    -- 2. 解析为 [BangumiDataItem]
    -- 3. 过滤匹配 targetSeason 的项
    ...
```

### 4. App 层集成

修改文件：`src/Moe/App/Monad.hs`

```haskell
type AppEffects =
  '[ Bangumi,
     BangumiData,  -- 新增
     SQLite,
     Error AppError,
     IOE
   ]
```

修改文件：`src/Moe/App/Run.hs`

```haskell
runApp env app =
  app
    & runBangumiSQLite
    & runBangumiDataHttp  -- 新增
    & runSQLiteWithPath env.databasePath
    & runErrorNoCallStack
    & runEff
```

### 5. 依赖添加

修改文件：`moe-bangumi.cabal`

```cabal
build-depends:
  ...
  aeson ^>=2.2,
  req ^>=3.13,
  time,
  http-client,
  http-client-tls,
```

### 6. Schema 更新

修改文件：`src/Moe/Infra/Persistence/Schema.hs`

- 添加 `MikanId` 的 `FromField`/`ToField` 实例
- 添加 `Season` 的 `FromField`/`ToField` 实例
- 更新 `Bangumi` 的 `FromRow`/`ToRow` 实例

## 文件变更清单

| 操作 | 文件路径 |
|------|---------|
| 新建 | `src/Moe/Core/Model/Season.hs` |
| 新建 | `src/Moe/Core/Model/BangumiData.hs` |
| 修改 | `src/Moe/Core/Model/Bangumi.hs` |
| 修改 | `src/Moe/Core/Model/Internal/Types.hs` |
| 新建 | `src/Moe/Effect/BangumiData.hs` |
| 新建 | `src/Moe/Infra/Effect/BangumiData.hs` |
| 修改 | `src/Moe/Infra/Persistence/Schema.hs` |
| 修改 | `src/Moe/App/Monad.hs` |
| 修改 | `src/Moe/App/Run.hs` |
| 修改 | `moe-bangumi.cabal` |

## 实现顺序

1. **添加依赖** - cabal 文件添加 aeson, req, time
2. **Core 层**
   - 创建 Season 模块
   - 更新 Internal/Types（添加 MikanId）
   - 更新 Bangumi 模型
   - 创建 BangumiData 模块
3. **Effect 层** - 创建 BangumiData Effect
4. **Infra 层**
   - 创建 BangumiData HTTP 实现
   - 更新 Schema
5. **App 层** - 集成到 Effect 栈
6. **测试** - 添加单元测试

## 验证方案

1. **构建验证**
   ```bash
   cabal build
   ```

2. **单元测试**
   - Season 解析测试
   - BangumiData JSON 解析测试
   - ID 提取测试
   - toBangumi 转换测试

3. **集成测试**
   ```haskell
   -- 在 GHCi 中测试
   items <- runApp defaultAppEnv $ fetchBangumiDataBySeason (Season 2026 Q1)
   print (length items)
   ```
