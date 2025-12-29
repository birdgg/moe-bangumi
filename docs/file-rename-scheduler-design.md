# File Rename Scheduler Design

## Objective
To implement a background scheduler that automatically renames downloaded media files (torrents) to be compatible with Plex/Jellyfin naming standards.

## Target Naming Convention
**Plex Standard:**
- **TV Shows:** `Show Name (Year)/Season XX/Show Name - sXXeYY.ext`
- **Movies:** `Movie Name (Year)/Movie Name (Year).ext`

The scheduler will support:
1.  **Single Episodes:** Renaming the main video file and associated subtitles.
2.  **Season Packs:** Identifying and renaming multiple episodes within a folder.
3.  **Subtitles:** Renaming external subtitle files (`.ass`, `.srt`, etc.) to match the video filename.
4.  **Metadata:** Generating `.nfo` files for better scraper compatibility.

## Architecture

### 1. Components
*   **`RenameJob`**: A new implementation of `SchedulerJob` in `crates/server/src/services/scheduler`.
*   **`DownloaderService`**: Enhanced to support file renaming operations.
*   **`Parser`**: Existing `crates/parser` to extract season/episode info from filenames.
*   **`PathGen`**: Existing `crates/pathgen` to generate standard filenames.

### 2. Downloader Trait Updates
The `Downloader` trait in `crates/downloader` needs a new method:
```rust
async fn rename_file(&self, id: &str, old_path: &str, new_path: &str) -> Result<()>;
```
*   **qBittorrent**: Supports `renameFile` API.
*   **Transmission**: Supports `torrent-rename-path` RPC method.

## Logic Flow

### Step 1: Task Discovery
The job runs periodically (e.g., every 5 minutes) and queries the downloader for tasks that:
1.  Have the status `Completed` or `Seeding`.
2.  Have the tag `moe` (indicating it was added by this system).
3.  Do **not** have the tag `renamed`.

### Step 2: Content Analysis
For each eligible task, fetch the file list:

#### Scenario A: Single Episode
*   **Condition**: Task contains one major video file (ignoring samples/extras).
*   **Action**:
    1.  Parse the original filename using `Parser` to confirm `season` and `episode`.
    2.  Generate target filename using `PathGen` (e.g., `Show - s01e01.mkv`).
    3.  Rename the video file.
    4.  Find matching subtitles (same basename) and rename them (e.g., `Show - s01e01.zh-CN.ass`).

#### Scenario B: Season Pack (Collection)
*   **Condition**: Task contains multiple video files.
*   **Action**:
    1.  Iterate through all video files.
    2.  Parse each filename to determine `episode` number (assuming `season` is constant or detectable).
    3.  Generate target filename for each.
    4.  Rename files locally within the torrent structure.
    *   *Note: Handling directory structure changes in torrents can be risky; we might restrict renaming to filenames only, keeping the folder structure.*

### Step 3: External File Generation (.nfo)
For each video file, generate a compatible `.nfo` file containing:
-   Show Title
-   Season/Episode Number
-   Original Filename (for reference)
-   Bangumi/TMDB ID

### Step 4: Finalization
1.  Upon successful renaming, remove the `renamed` tag to the torrent task.
2.  (Optional) Send a notification or log the event.

## Implementation Plan

1.  **Update `Downloader` Trait**: Add `rename_file` support.
2.  **Implement `RenameJob`**:
    -   Fetch tasks.
    -   Logic to distinguish Single vs. Pack.
    -   Integration with `Parser` and `PathGen`.
3.  **Subtitle Handling**: Logic to find and rename sidecar files.
4.  **NFO Generator**: Simple XML/Text generation for `.nfo`.
5.  **Testing**: Unit tests for the renaming logic using mock file lists.

## Safety Mechanisms
-   **Dry Run**: Log what would happen without executing first.
-   **Extension Check**: Never change the file extension (except for `.nfo` generation).
-   **Conflict Avoidance**: Check if the target filename already exists.
