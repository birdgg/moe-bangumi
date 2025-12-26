//! File rename service for downloaded torrents.
//!
//! This service renames downloaded video files to Plex/Jellyfin compatible names.
//! It uses a strategy pattern to handle different torrent types (episode vs collection).

mod strategy;

use sqlx::SqlitePool;
use std::sync::Arc;

use crate::models::{Bangumi, DownloadTask, DownloadTaskStatus, Torrent, TorrentKind};
use crate::repositories::BangumiRepository;
use crate::services::DownloaderService;

pub use strategy::{CollectionRenameStrategy, EpisodeRenameStrategy, RenameStrategy};

/// Result of a file rename operation
#[derive(Debug)]
pub enum RenameResult {
    /// File was renamed successfully
    Renamed { old_path: String, new_path: String },
    /// File already has the correct name
    AlreadyRenamed,
    /// No video file found in torrent
    NoVideoFile,
    /// Task was already completed
    AlreadyCompleted,
    /// Failed to rename a specific file
    Failed { path: String, error: String },
    /// Failed to parse episode number from filename
    ParseFailed { path: String, reason: String },
}

/// Error type for file rename operations
#[derive(Debug, thiserror::Error)]
pub enum FileRenameError {
    #[error("Bangumi {0} not found")]
    BangumiNotFound(i64),
    #[error("Invalid torrent: {0}")]
    InvalidTorrent(String),
    #[error("Collection rename failed: {0}")]
    CollectionRenameFailed(String),
    #[error("Database error: {0}")]
    Database(#[from] sqlx::Error),
    #[error("Downloader error: {0}")]
    Downloader(#[from] downloader::DownloaderError),
}

/// Service for renaming downloaded files to Plex/Jellyfin compatible names.
///
/// This service uses a strategy pattern to handle different torrent types:
/// - Episode torrents: Rename the single main video file
/// - Collection torrents: Parse and rename multiple video files
pub struct FileRenameService;

impl FileRenameService {
    /// Get the appropriate rename strategy for a torrent.
    fn get_strategy(torrent: &Torrent) -> Box<dyn RenameStrategy> {
        match torrent.kind {
            TorrentKind::Episode => Box::new(EpisodeRenameStrategy),
            TorrentKind::Collection => Box::new(CollectionRenameStrategy),
        }
    }

    /// Rename files in a completed torrent.
    ///
    /// This function:
    /// 1. Gets the bangumi info for path generation
    /// 2. Selects the appropriate rename strategy based on torrent kind
    /// 3. Executes the rename strategy
    /// 4. Updates the task status in the database
    pub async fn rename_completed_torrent(
        db: &SqlitePool,
        downloader: &Arc<DownloaderService>,
        torrent: &Torrent,
        task: &DownloadTask,
    ) -> Result<Vec<RenameResult>, FileRenameError> {
        // Get bangumi info for path generation
        let bangumi = BangumiRepository::get_by_id(db, torrent.bangumi_id)
            .await?
            .ok_or(FileRenameError::BangumiNotFound(torrent.bangumi_id))?;

        Self::rename_with_bangumi(db, downloader, torrent, task, &bangumi).await
    }

    /// Rename with pre-fetched bangumi info.
    ///
    /// Use this variant when you already have the bangumi loaded to avoid
    /// an extra database query.
    pub async fn rename_with_bangumi(
        db: &SqlitePool,
        downloader: &Arc<DownloaderService>,
        torrent: &Torrent,
        task: &DownloadTask,
        bangumi: &Bangumi,
    ) -> Result<Vec<RenameResult>, FileRenameError> {
        // Skip if already completed
        if task.status == DownloadTaskStatus::Completed {
            tracing::debug!("Task {} already completed, skipping", task.id);
            return Ok(vec![RenameResult::AlreadyCompleted]);
        }

        // Get the appropriate strategy and execute
        let strategy = Self::get_strategy(torrent);
        strategy.rename(db, downloader, torrent, task, bangumi).await
    }
}
