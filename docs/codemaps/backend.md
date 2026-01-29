# 后端结构

> 最后更新: 2026-01-30

## 模块树

```
src/Moe/
+-- Domain/
|   +-- Bangumi/
|   |   +-- Types.hs              # 番剧领域模型
|   +-- File/
|   |   +-- Types.hs              # 文件类型定义
|   |   +-- Naming.hs             # 文件命名规则
|   |   +-- Parser.hs             # 标题解析器 API
|   +-- Internal/
|       +-- File/
|           +-- Group.hs          # 字幕组识别
|           +-- TitleParser.hs    # 标题解析实现
|           +-- Parser/
|               +-- Types.hs
|               +-- Bracket.hs
|               +-- EpisodePattern.hs
|
+-- Effect/
|   +-- Bangumi.hs                # BangumiQuery, BangumiUpdate
|   +-- BangumiData.hs            # BangumiData
|   +-- Metadata.hs               # Metadata（多源搜索）
|
+-- Adapter/
|   +-- Database/
|   |   +-- Bangumi.hs            # runBangumiQuerySQLite, runBangumiUpdateSQLite
|   |   +-- Orphans.hs            # 序列化实例
|   +-- Http/
|       +-- BangumiData.hs        # runBangumiDataHttp
|       +-- Metadata.hs           # runMetadataHttp
|
+-- Infra/
|   +-- BangumiData/
|       +-- Types.hs              # API 响应类型
|       +-- Client.hs             # HTTP 客户端
|
+-- App/
|   +-- Env.hs                    # MoeEnv 运行时配置
|   +-- Error.hs                  # MoeError 类型
|   +-- Monad.hs                  # MoeM 类型别名, runMoe
|   +-- Logging.hs                # 日志配置
|
+-- Web/
    +-- Server.hs                 # Warp 服务器
    +-- Routers.hs                # 路由组合
    +-- Types.hs                  # Web 层类型
    +-- Scalar.hs                 # OpenAPI 文档
    +-- API/
        +-- Routes.hs             # API 路由定义
        +-- Server.hs             # API Handler
```

## Effect 系统

### Effect 定义

| Effect | 模块 | 操作 |
|--------|------|------|
| BangumiQuery | `Moe.Effect.Bangumi` | `getBangumi`, `listBangumi` |
| BangumiUpdate | `Moe.Effect.Bangumi` | `createBangumi`, `upsertBangumi` |
| BangumiData | `Moe.Effect.BangumiData` | `fetchBangumiByMonths` |
| Metadata | `Moe.Effect.Metadata` | `searchBgmtv`, `searchTmdb` |

### Effect 解释器

| 解释器 | 模块 | Effect |
|--------|------|--------|
| `runBangumiQuerySQLite` | `Moe.Adapter.Database.Bangumi` | BangumiQuery |
| `runBangumiUpdateSQLite` | `Moe.Adapter.Database.Bangumi` | BangumiUpdate |
| `runBangumiDataHttp` | `Moe.Adapter.Http.BangumiData` | BangumiData |
| `runMetadataHttp` | `Moe.Adapter.Http.Metadata` | Metadata |

## 关键模块职责

| 模块 | 职责 |
|------|------|
| `Domain.Bangumi.Types` | 番剧数据类型（Bangumi, AnimeSeason, 各种 ID） |
| `Domain.File.Types` | 文件相关类型（BangumiFile, FileType, SubtitleLang） |
| `Domain.File.Naming` | 生成符合媒体服务器规范的文件路径 |
| `Domain.File.Parser` | 从文件名解析番剧信息 |
| `App.Monad` | MoeM 类型和 runMoe 执行函数 |
| `App.Env` | 运行时环境配置（数据库路径、日志配置） |

## 导入模式

```haskell
-- Domain 层：无外部依赖
import Moe.Domain.Bangumi.Types
import Moe.Domain.File.Types

-- Effect 层：依赖 Domain
import Moe.Domain.Bangumi.Types qualified as Types
import Moe.Effect.Bangumi

-- Adapter 层：依赖 Domain + Effect
import Moe.Domain.Bangumi.Types qualified as Types
import Moe.Effect.Bangumi (BangumiQuery (..))
import Effectful

-- App 层：组合所有层
import Moe.Effect.Bangumi (BangumiQuery, BangumiUpdate)
import Moe.Adapter.Database.Bangumi (runBangumiQuerySQLite)
```
