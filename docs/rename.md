# Rename Flow

Rename job processes completed torrents tagged with `rename`, dispatches to subscription or collection strategy based on tags.

## Overview

```mermaid
flowchart TD
    A[Torrent added] -->|auto-tagged: moe + rename| B[Download to tmp/]
    B --> C{Download complete?}
    C -->|No| C
    C -->|Yes| D[Rename Job picks up]
    D --> E{Strategy by tag}
    E -->|subscription| F[Subscription Strategy]
    E -->|collection| H[Collection Strategy]
    E -->|neither| I[Skip]
```

## Subscription Strategy

Single-file torrents from RSS. The torrent name is already formatted by the subscription job, only needs file extension appended.

```mermaid
sequenceDiagram
    participant RJ as Rename Job
    participant QB as qBittorrent

    RJ->>QB: getRenameTorrents
    QB-->>RJ: [TorrentInfo] (completed + rename tag)

    loop Each torrent (sequential)
        RJ->>QB: getTorrentFiles(hash)
        QB-->>RJ: [TorrentFile]

        alt Single file
            RJ->>QB: renameTorrentFile(hash, old, name + ext)
            RJ->>QB: setTorrentLocation(hash, savePath)
            RJ->>QB: removeTagsFromTorrents(hash, [rename])
            RJ->>RJ: notify
        else Multi-file
            RJ->>RJ: log warning, skip
        end
    end
```

### Path Transformation

```
tmp:   /media/tmp/Title (2025)/Season 01/[Sub] Title - 01.mkv
                                          ^^^^^^^^^^^^^^^^^^^^ old filename

final: /media/bangumi/Title (2025)/Season 01/Title (2025) - S01E01.mkv
                                              ^^^^^^^^^^^^^^^^^^^^^^^^ torrent.name + ext
```

`computeTargetLocation` strips the `tmp` prefix and prepends `savePath`:

```
currentPath: /media/tmp/Title (2025)/Season 01/
tmpBase:     /media/tmp
savePath:    /media/bangumi
result:      /media/bangumi/Title (2025)/Season 01/
```

## Collection Strategy

Multi-file torrents (e.g. BD-Rip collections). Resolves metadata from bgm.tv, renames all files to media server conventions, then persists to database.

```mermaid
sequenceDiagram
    participant RJ as Rename Job
    participant QB as qBittorrent
    participant BGM as bgm.tv API
    participant DB as SQLite

    RJ->>QB: getTorrentFiles(hash)
    QB-->>RJ: [TorrentFile]

    note over RJ: foldM - sequential processing<br/>carries BangumiMap cache

    loop Each file (sequential via foldM)
        RJ->>RJ: findBangumiDir(file)
        alt Dir found & not in cache
            RJ->>RJ: parseCollectionDirName(dir)
            RJ->>BGM: searchBgmtv(keyword)
            BGM-->>RJ: Maybe Bangumi
            RJ->>RJ: cache result in BangumiMap
        else Dir found & in cache
            RJ->>RJ: use cached result
        end

        alt Video file
            RJ->>RJ: buildVideoPath(bangumi, file)
        else Non-video file
            RJ->>RJ: buildNonVideoPath(bangumi, relPath)
        end
        RJ->>QB: renameTorrentFile(hash, old, new)
    end

    RJ->>DB: persistBangumiMap (upsert bangumi + tracking)
    RJ->>QB: setTorrentLocation(hash, savePath)
    RJ->>QB: removeTagsFromTorrents(hash, [rename])
```

### BangumiMap Cache

`foldM` processes files sequentially to carry a `BangumiMap` (metadata cache) between iterations. This avoids redundant bgm.tv API calls for files under the same bangumi directory.

```mermaid
flowchart LR
    F1[File 1: Anime_A/01.mkv] -->|resolve Anime_A| API1[bgm.tv search]
    API1 -->|cache| BM1[BangumiMap: Anime_A -> Bangumi]
    F2[File 2: Anime_A/02.mkv] -->|cache hit| BM1
    F3[File 3: Anime_B/01.mkv] -->|resolve Anime_B| API2[bgm.tv search]
    API2 -->|cache| BM2[BangumiMap: Anime_A, Anime_B]
```

### Video File Classification

```mermaid
flowchart TD
    V[Video file] --> SP{parseSpContent}
    SP -->|ExtraContent: NCOP, NCED, PV...| EX[/extras]
    SP -->|SpecialEpisode: SP01| S0[/Season 00]
    SP -->|Nothing| PI[parseInfo]
    PI --> EP[/Season XX]
```

### Naming Convention

Rename output follows media server conventions (Plex/Emby/Jellyfin):

| Type            | Directory       | Filename Format                   | Example                               |
| --------------- | --------------- | --------------------------------- | ------------------------------------- |
| Regular Episode | `/Season XX`    | `Title - SxxExx [Group].mkv`      | `Frieren - S01E01 [SubGroup].mkv`     |
| Special         | `/Season 00`    | `Title - S00Exx [Group].mkv`      | `Frieren - S00E01 [SubGroup].mkv`     |
| NCOP            | `/extras`       | `NCOPx.mkv`                       | `NCOP1.mkv`                           |
| NCED            | `/extras`       | `NCEDx.mkv`                       | `NCED1.mkv`                           |
| Menu            | `/extras`       | `Menu.mkv`                        | `Menu.mkv`, `Menu Vol.1.mkv`          |
| PV              | `/trailers`     | `PVx.mkv`                         | `PV1.mkv`                             |
| Preview         | `/trailers`     | `Preview.mkv`                     | `Preview.mkv`                         |
| Trailer         | `/trailers`     | `Trailer.mkv`                     | `Trailer.mkv`                         |
| CM              | `/trailers`     | `CMx.mkv`                         | `CM1.mkv`                             |
| Movie           | `/Title (Year)` | `Title (Year).mkv`                | `Frieren Movie (2024).mkv`            |
| Subtitle        | Same as video   | `Title - SxxExx [Group].lang.ext` | `Frieren - S01E01 [SubGroup].zh-Hans.srt` |

Cds, Scans 等非视频目录不处理，直接移动。

### Special Directories (Skipped)

Files in these directories are not treated as bangumi content:

`cds` `cd` `sps` `sp` `scans` `scan` `menu` `menus` `fonts` `font` `extras` `extra` `trailers` `trailer` `pvs` `pv` `specials` `special` `bonus` `ost` `soundtrack` `soundtracks`

### Database Persistence

After renaming all files, resolved bangumi are persisted in a single transaction:

```mermaid
flowchart LR
    BM[BangumiMap] --> TX[SQLite Transaction]
    TX --> UB[upsertBangumi]
    TX --> UT[upsertTracking<br/>type=Collection]
```

## Error Handling

```mermaid
flowchart TD
    T[Process torrent] --> TRY{try}
    TRY -->|Success| DONE[Continue to next]
    TRY -->|AsyncException| THROW[Re-throw, stop job]
    TRY -->|Other Exception| LOG[Log error, continue to next]
```

- Each torrent is wrapped in `try @SomeException` -- a failure does not block other torrents
- Async exceptions are re-thrown (cancellation must propagate)
- `notifySafe` catches notification errors independently -- rename succeeds even if notification fails

## Module Map

```
src/Moe/Job/
  Rename.hs              -- Entry point, job definition, subscription strategy
  Rename/
    Collection.hs        -- Collection strategy
    Util.hs              -- computeTargetLocation, notifySafe

src/Moe/Infra/Downloader/
    Effect.hs            -- Downloader effect (renameTorrentFile, setTorrentLocation...)
    Types.hs             -- Tag constants, TorrentInfo, AddTorrentParams
    Adapter.hs           -- qBittorrent implementation
```
