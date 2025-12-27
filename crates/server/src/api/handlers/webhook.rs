use axum::extract::{Query, State};
use serde::Deserialize;
use utoipa::IntoParams;

use crate::error::{AppError, AppResult};
use crate::repositories::{DownloadTaskRepository, TorrentRepository};
use crate::services::{FileRenameError, FileRenameService, RenameResult};
use crate::state::AppState;

/// Query parameters for torrent completion webhook
#[derive(Debug, Deserialize, IntoParams)]
pub struct TorrentCompletedQuery {
    /// Torrent info hash from qBittorrent
    pub hash: String,
    /// Torrent name
    pub name: String,
}

/// Webhook endpoint for qBittorrent torrent completion callback.
///
/// Configure qBittorrent to call this endpoint when a torrent finishes downloading:
/// Settings -> Downloads -> "Run external program on torrent finished"
/// Command: `curl -X POST "http://your-server:3000/api/webhook/torrent-completed?hash=%I"`
///
/// The `%I` placeholder will be replaced with the torrent's info hash.
#[utoipa::path(
    post,
    path = "/api/webhook/torrent-completed",
    tag = "webhook",
    params(TorrentCompletedQuery),
    responses(
        (status = 200, description = "File renamed successfully"),
        (status = 404, description = "Torrent not found")
    )
)]
pub async fn torrent_completed(
    State(state): State<AppState>,
    Query(query): Query<TorrentCompletedQuery>,
) -> AppResult<&'static str> {
    let hash = query.hash.to_lowercase();
    tracing::info!("Received torrent completion webhook for hash: {}", hash);

    // Find the torrent by info_hash
    let torrent = TorrentRepository::get_by_info_hash(&state.db, &hash)
        .await
        .map_err(|e| AppError::internal(e.to_string()))?
        .ok_or_else(|| {
            tracing::warn!("Torrent not found for hash: {}", hash);
            AppError::not_found(format!("Torrent with hash {} not found", hash))
        })?;

    // Find the downloading task for this torrent
    let task = DownloadTaskRepository::get_latest_by_torrent_id(&state.db, torrent.id)
        .await
        .map_err(|e| AppError::internal(e.to_string()))?
        .ok_or_else(|| {
            tracing::warn!("No download task found for torrent: {}", torrent.id);
            AppError::not_found(format!("No download task for torrent {}", torrent.id))
        })?;

    // Use shared rename service (now returns Vec<RenameResult>)
    match FileRenameService::rename_completed_torrent(&state.db, &state.downloader, &torrent, &task)
        .await
    {
        Ok(results) => {
            // Summarize results for response
            let renamed_count = results
                .iter()
                .filter(|r| matches!(r, RenameResult::Renamed { .. }))
                .count();
            let already_renamed_count = results
                .iter()
                .filter(|r| matches!(r, RenameResult::AlreadyRenamed))
                .count();
            let failed_count = results
                .iter()
                .filter(|r| {
                    matches!(r, RenameResult::Failed { .. } | RenameResult::ParseFailed { .. })
                })
                .count();

            if torrent.is_collection() {
                tracing::info!(
                    "Collection torrent completed: {} renamed, {} already renamed, {} failed",
                    renamed_count,
                    already_renamed_count,
                    failed_count
                );
                Ok(if renamed_count > 0 || already_renamed_count > 0 {
                    "Collection processed successfully"
                } else {
                    "Collection completed (no video files)"
                })
            } else {
                Ok(match results.first() {
                    Some(RenameResult::Renamed { .. }) => "File renamed successfully",
                    Some(RenameResult::AlreadyRenamed) => "Already renamed",
                    Some(RenameResult::NoVideoFile) => "Completed (no video file)",
                    Some(RenameResult::AlreadyCompleted) => "Already completed",
                    Some(RenameResult::Failed { .. }) => "Failed to rename file",
                    Some(RenameResult::ParseFailed { .. }) => "Failed to parse filename",
                    None => "No operation performed",
                })
            }
        }
        Err(FileRenameError::BangumiNotFound(id)) => {
            Err(AppError::internal(format!("Bangumi {} not found", id)))
        }
        Err(FileRenameError::InvalidTorrent(e)) => {
            Err(AppError::bad_request(format!("Invalid torrent: {}", e)))
        }
        Err(FileRenameError::CollectionRenameFailed(e)) => {
            Err(AppError::internal(format!("Collection rename failed: {}", e)))
        }
        Err(FileRenameError::Database(e)) => Err(AppError::internal(e.to_string())),
        Err(FileRenameError::Downloader(e)) => {
            Err(AppError::internal(format!("Failed to rename file: {}", e)))
        }
    }
}
