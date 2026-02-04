# 设计文档

## 文件与目录规范

### 命名规范总表

| 类型 | 目录 | 文件名格式 | 示例 |
|------|------|------------|------|
| 正常剧集 | `/Season XX` | `作品名称 - SxxExx [字幕组].mkv` | `葬送的芙莉莲 - S01E01 [喵萌奶茶屋].mkv` |
| 特别篇 | `/Season 00` | `作品名称 - S00Exx [字幕组].mkv` | `葬送的芙莉莲 - S00E01 [喵萌奶茶屋].mkv` |
| NCOP | `/extras` | `NCOPx.mkv` | `NCOP1.mkv` |
| NCED | `/extras` | `NCEDx.mkv` | `NCED1.mkv` |
| Menu | `/extras` | `Menu.mkv` | `Menu.mkv`, `Menu Vol.1.mkv` |
| PV | `/trailers` | `PVx.mkv` | `PV1.mkv` |
| Preview | `/trailers` | `Preview.mkv` | `Preview.mkv` |
| Trailer | `/trailers` | `Trailer.mkv` | `Trailer.mkv` |
| CM | `/trailers` | `CMx.mkv` | `CM1.mkv` |
| 剧场版 | `/作品名称 (年份)` | `作品名称 (年份).mkv` | `葬送的芙莉莲 剧场版 (2024).mkv` |
| 字幕 | 同视频目录 | `作品名称 - SxxExx [字幕组].lang.ext` | `葬送的芙莉莲 - S01E01 [喵萌奶茶屋].chs.srt` |

目录归属规则：
- **`/Season XX`** = 正常剧集
- **`/Season 00`** = 特别篇（OVA/OAD/SP）
- **`/extras`** = 片头片尾、菜单（Credit-less 内容）
- **`/trailers`** = 宣传视频、预告片、广告

### 元数据 ID 标记

| 数据源 | 格式 |
|--------|------|
| TMDB | `{tmdbid-12345}` |


示例：`/葬送的芙莉莲 (2023) {tmdbid-418124}`

### 最佳实践

1. **保持一致性**：整个库使用统一命名格式
2. **包含年份**：避免同名作品混淆
3. **使用元数据 ID**：对于难以匹配的作品
4. **避免特殊字符**：不使用 `< > : " / \ | ? *`
5. **使用两位数**：`S01E01` 而非 `S1E1`

### 参考资料

- [Plex TV Show Naming](https://support.plex.tv/articles/naming-and-organizing-your-tv-show-files/)
- [Emby TV Naming](https://emby.media/support/articles/TV-Naming.html)
- [Jellyfin Shows](https://jellyfin.org/docs/general/server/media/shows/)
- [AniDB Episodes](https://wiki.anidb.net/Content:Episodes)

## 番剧日历
程序通过 [bangumi-data](https://github.com/bangumi-data/bangumi-data) 获取每个季度的番剧，bangumi-data 数据结构参考 @bangumi-data.md

## 订阅番剧
获取 tacking 表 enabled rss 和关联的 bangumi，每个 rss url 对应一部番剧。

对 rss 处理并发执行，5个一组

对于每个 rss 使用以下步骤
把 rss 解析为 {title, torrentUrl, pubdate}
根据 tacking pubdate 过滤已经处理的 item
根据用户提供全局正则过滤符合 title 的 item

对每集 title 使用 RssTitle parser 解析
和数据库记录每一集和新的 rss item 放在一起洗版出优先的字幕组剧集

添加到下载器是使用 rename 参数重命名 torrent name 为符合规范的文件名，保存路径为临时目录下符合目录规范，添加 tag `subscription`

## 番剧合集
用户通过 nyaa， acgrip rss 搜索，直接添加合集 torrent 到下载器

## 下载器
使用 qbittorrent 作为下载器
添加到下载器的时候应该在一个临时目录 tmp，在重命名的时候转移到用户番剧目录
添加到下载器的时候添加 tag [moe,rename]
通过订阅添加时，添加tag subscribe
通过合集添加时，添加tag collection

## 重命名
通过 qb api 获取已完成，含有 rename tag 的列表， 重命名完成后删除 rename tag

### 订阅
含有 subscribe tag，torrent name 为文件名，目录直接可以从当前的目录路径获取到

### 合集
含有 collection tag
```json
        "name": "[VCB-Studio] Re Zero kara Hajimeru Isekai Seikatsu/[VCB-Studio] Re Zero kara Hajimeru Isekai Seikatsu 2nd Season [Ma10p_1080p]/CDs/[210428] ORIGINAL SOUND TRACK 2／末廣健一郎 (flac+webp)/01. 英雄のタクト -覚醒-.flac"
```

需要解析字符串为路径格式
建立查询表，如果没有则通过 tmdb api 查询
按照`文件与目录规范`进行重命名
