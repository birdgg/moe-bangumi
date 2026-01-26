# 番剧命名规范

本文档整理了 Plex、Emby、Jellyfin 对于番剧（动画）的命名规范。

## 命名规范总表

| 类型 | 目录 | 文件名格式 | 示例 |
|------|------|------------|------|
| 正常剧集 | `/Season XX` | `作品名称 - SxxExx.mkv` | `葬送的芙莉莲 - S01E01.mkv` |
| 特别篇 | `/Season 00` | `作品名称 - S00Exx.mkv` | `葬送的芙莉莲 - S00E01.mkv` |
| NCOP | `/extras` | `NCOPx.mkv` | `NCOP1.mkv` |
| NCED | `/extras` | `NCEDx.mkv` | `NCED1.mkv` |
| Menu | `/extras` | `Menu.mkv` | `Menu.mkv`, `Menu Vol.1.mkv` |
| PV | `/trailers` | `PVx.mkv` | `PV1.mkv` |
| Preview | `/trailers` | `Preview.mkv` | `Preview.mkv` |
| Trailer | `/trailers` | `Trailer.mkv` | `Trailer.mkv` |
| CM | `/trailers` | `CMx.mkv` | `CM1.mkv` |
| 剧场版 | `/作品名称 (年份)` | `作品名称 (年份).mkv` | `葬送的芙莉莲 剧场版 (2024).mkv` |
| 字幕 | 同视频目录 | `作品名称 - SxxExx.lang.ext` | `葬送的芙莉莲 - S01E01.chs.srt` |

目录归属规则：
- **`/Season XX`** = 正常剧集
- **`/Season 00`** = 特别篇（OVA/OAD/SP）
- **`/extras`** = 片头片尾、菜单（Credit-less 内容）
- **`/trailers`** = 宣传视频、预告片、广告

## 元数据 ID 标记

| 数据源 | 格式 |
|--------|------|
| TMDB | `{tmdbid-12345}` |


示例：`/葬送的芙莉莲 (2023) {tmdbid-418124}`

## 最佳实践

1. **保持一致性**：整个库使用统一命名格式
2. **包含年份**：避免同名作品混淆
3. **使用元数据 ID**：对于难以匹配的作品
4. **避免特殊字符**：不使用 `< > : " / \ | ? *`
5. **使用两位数**：`S01E01` 而非 `S1E1`

## 参考资料

- [Plex TV Show Naming](https://support.plex.tv/articles/naming-and-organizing-your-tv-show-files/)
- [Emby TV Naming](https://emby.media/support/articles/TV-Naming.html)
- [Jellyfin Shows](https://jellyfin.org/docs/general/server/media/shows/)
- [AniDB Episodes](https://wiki.anidb.net/Content:Episodes)
