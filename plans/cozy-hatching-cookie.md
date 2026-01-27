# moe-bangumi 文件命名逻辑实现计划

## 概述

为 moe-bangumi 项目实现文件命名生成逻辑，支持 Plex/Emby/Jellyfin 媒体服务器的番剧文件命名规范。

## 模块结构

```
src/Bangumi/
├── Internal/
│   ├── Subtitle.hs      # [已存在]
│   ├── Content.hs       # 内容类型定义
│   ├── Extension.hs     # 文件扩展名
│   ├── BangumiFile.hs   # 完整文件模型
│   └── Naming.hs        # 命名生成逻辑
└── File.hs              # 公开 API
```

## Phase 1: 基础类型定义

### 1.1 创建 `src/Bangumi/Internal/Extension.hs`

定义视频和字幕文件扩展名：

```haskell
data VideoExt = MKV | MP4 | AVI | WEBM
data SubtitleExt = SRT | ASS | SSA | SUB
data FileType = Video VideoExt | Subtitle SubtitleLang SubtitleExt
```

### 1.2 创建 `src/Bangumi/Internal/Content.hs`

定义内容类型和相关 newtype：

```haskell
newtype SeasonNum = SeasonNum Word8
newtype EpisodeNum = EpisodeNum Word8
newtype Index = Index Word8
newtype VolumeNum = VolumeNum Word8
newtype Year = Year Word16
newtype TmdbId = TmdbId Word32

data EpisodeType = Regular SeasonNum EpisodeNum | Special EpisodeNum
data ExtraContent = NCOP (Maybe Index) | NCED (Maybe Index) | Menu (Maybe VolumeNum)
data TrailerContent = PV Index | Preview | Trailer | CM (Maybe Index)
data BangumiContent = Episode EpisodeType | Extra ExtraContent | TrailerItem TrailerContent | Movie Year
```

## Phase 2: 组合类型

### 2.1 创建 `src/Bangumi/Internal/BangumiFile.hs`

```haskell
data BangumiMeta = BangumiMeta { name :: Text, year :: Maybe Year, tmdbId :: Maybe TmdbId }
data BangumiFile = BangumiFile { meta :: BangumiMeta, content :: BangumiContent, fileType :: FileType }
```

## Phase 3: 命名逻辑

### 3.1 创建 `src/Bangumi/Internal/Naming.hs`

核心函数：
- `generatePath :: BangumiFile -> FilePath` - 生成目录路径
- `generateFileName :: BangumiFile -> FilePath` - 生成文件名
- `generateFullPath :: BangumiFile -> FilePath` - 完整路径
- `sanitizeName :: Text -> Text` - 清理非法字符

命名规则摘要：
| 内容类型 | 目录 | 文件名格式 |
|---------|------|-----------|
| 正常剧集 | `/Season XX` | `作品名 - SxxExx.mkv` |
| 特别篇 | `/Season 00` | `作品名 - S00Exx.mkv` |
| NCOP/NCED/Menu | `/extras` | `NCOPx.mkv` |
| PV/Preview/CM | `/trailers` | `PVx.mkv` |
| 剧场版 | `/作品名 (年)` | `作品名 (年).mkv` |
| 字幕 | 同视频 | `作品名 - SxxExx.lang.ext` |

## Phase 4: 公开 API

### 4.1 修改 `src/Bangumi/File.hs`

重导出所有公开类型和函数。

## Phase 5: 测试

### 5.1 扩展 `test/Main.hs`

测试场景：
- 正常剧集命名 (S01E01, S02E15)
- 特别篇命名 (S00E01)
- 附加内容 (NCOP, NCED, Menu)
- 宣传内容 (PV, Preview, Trailer, CM)
- 剧场版
- 字幕文件
- 特殊字符清理 (`:` → `-`, `/` → `-`, 移除 `<>|?*`)
- 边界情况 (无年份、有 TMDB ID)

## 关键文件

- `src/Bangumi/Internal/Content.hs` - 核心数据类型
- `src/Bangumi/Internal/Naming.hs` - 命名逻辑
- `src/Bangumi/File.hs` - 公开 API
- `test/Main.hs` - 测试

## 验证方法

1. `cabal build` - 确保编译通过
2. `cabal test` - 运行测试
3. `cabal repl` 手动测试示例：
   ```haskell
   import Bangumi.File
   let meta = BangumiMeta "葬送的芙莉莲" (Just 2023) Nothing
   let file = BangumiFile meta (Episode (Regular 1 1)) (Video MKV)
   generateFullPath file
   -- 应输出: "葬送的芙莉莲 (2023)/Season 01/葬送的芙莉莲 - S01E01.mkv"
   ```

## 更新 cabal 文件

需要在 `exposed-modules` 和 `other-modules` 中添加新模块。
