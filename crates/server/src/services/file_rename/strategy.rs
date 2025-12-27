//! Rename strategy implementations for different torrent types.
//!
//! This module provides the strategy pattern for handling file renaming
//! based on torrent kind (episode vs collection).

use async_trait::async_trait;
use downloader::TorrentFile;
use sqlx::SqlitePool;
use std::sync::Arc;

use crate::models::{Bangumi, DownloadTask, Torrent};
use crate::repositories::DownloadTaskRepository;
use crate::services::DownloaderService;

use super::{FileRenameError, RenameResult};

/// Generate the new filename and path for a video file.
///
/// Returns (new_filename, new_path) where new_path preserves the original directory structure.
fn generate_rename_path(
    bangumi: &Bangumi,
    episode_number: i32,
    file: &TorrentFile,
) -> (String, String) {
    let target_filename = pathgen::generate_filename(
        &bangumi.title_chinese,
        bangumi.season,
        episode_number,
        Some(bangumi.platform.as_str()),
    );

    let extension = file.extension().unwrap_or("mkv");
    let new_filename = format!("{}.{}", target_filename, extension);

    let new_path = if let Some((dir, _)) = file.name.rsplit_once('/') {
        format!("{}/{}", dir, new_filename)
    } else {
        new_filename.clone()
    };

    (new_filename, new_path)
}

/// File rename strategy trait.
///
/// Defines how to rename files for different torrent types.
#[async_trait]
pub trait RenameStrategy: Send + Sync {
    /// Rename files in the torrent according to the strategy.
    async fn rename(
        &self,
        db: &SqlitePool,
        downloader: &Arc<DownloaderService>,
        torrent: &Torrent,
        task: &DownloadTask,
        bangumi: &Bangumi,
    ) -> Result<Vec<RenameResult>, FileRenameError>;
}

/// Single episode rename strategy.
///
/// Renames the largest video file to match the episode number.
pub struct EpisodeRenameStrategy;

#[async_trait]
impl RenameStrategy for EpisodeRenameStrategy {
    async fn rename(
        &self,
        db: &SqlitePool,
        downloader: &Arc<DownloaderService>,
        torrent: &Torrent,
        task: &DownloadTask,
        bangumi: &Bangumi,
    ) -> Result<Vec<RenameResult>, FileRenameError> {
        // Get the episode number (required for episode kind)
        let episode_number = torrent.episode_number.ok_or_else(|| {
            FileRenameError::InvalidTorrent("Episode torrent missing episode_number".to_string())
        })?;

        // Get files in the torrent
        let files = downloader.get_task_files(&torrent.info_hash).await?;

        // Find the main video file (largest completed video file)
        let main_file = files
            .iter()
            .filter(|f| f.is_completed() && f.is_video())
            .max_by_key(|f| f.size);

        let Some(main_file) = main_file else {
            tracing::warn!(
                "No completed video file found for task {}, marking as completed anyway",
                task.id
            );
            DownloadTaskRepository::mark_completed(db, task.id).await?;
            return Ok(vec![RenameResult::NoVideoFile]);
        };

        let (new_filename, new_path) = generate_rename_path(bangumi, episode_number, main_file);

        // Check if already renamed
        if main_file.name.ends_with(&new_filename) {
            tracing::debug!(
                "File already has correct name for task {}, marking as completed",
                task.id
            );
            DownloadTaskRepository::mark_completed(db, task.id).await?;
            return Ok(vec![RenameResult::AlreadyRenamed]);
        }

        // Rename the file
        match downloader
            .rename_file(&torrent.info_hash, &main_file.name, &new_path)
            .await
        {
            Ok(()) => {
                tracing::info!(
                    "Renamed file for task {}: {} -> {}",
                    task.id,
                    main_file.name,
                    new_path
                );
                DownloadTaskRepository::mark_completed(db, task.id).await?;
                Ok(vec![RenameResult::Renamed {
                    old_path: main_file.name.clone(),
                    new_path,
                }])
            }
            Err(e) => {
                tracing::error!("Failed to rename file for task {}: {}", task.id, e);
                DownloadTaskRepository::mark_failed(db, task.id, &e.to_string()).await?;
                Err(FileRenameError::Downloader(e))
            }
        }
    }
}

/// Collection rename strategy.
///
/// Parses filenames to extract episode numbers and renames each video file.
pub struct CollectionRenameStrategy;

#[async_trait]
impl RenameStrategy for CollectionRenameStrategy {
    async fn rename(
        &self,
        db: &SqlitePool,
        downloader: &Arc<DownloaderService>,
        torrent: &Torrent,
        task: &DownloadTask,
        bangumi: &Bangumi,
    ) -> Result<Vec<RenameResult>, FileRenameError> {
        // Get files in the torrent
        let files = downloader.get_task_files(&torrent.info_hash).await?;

        // Filter completed video files
        let video_files: Vec<_> = files
            .iter()
            .filter(|f| f.is_completed() && f.is_video())
            .collect();

        if video_files.is_empty() {
            tracing::warn!(
                "No completed video files found for collection task {}, marking as completed anyway",
                task.id
            );
            DownloadTaskRepository::mark_completed(db, task.id).await?;
            return Ok(vec![RenameResult::NoVideoFile]);
        }

        let total_files = video_files.len();
        tracing::info!(
            "Processing collection torrent {} with {} video files",
            torrent.id,
            total_files
        );

        // Parse each file and rename
        let parser = parser::Parser::new();
        let mut results = Vec::new();
        let mut rename_count = 0;

        for file in video_files {
            // Extract filename without directory path for parsing
            let filename = file.name.rsplit('/').next().unwrap_or(&file.name);

            // Parse episode number from filename
            match parser.parse(filename) {
                Ok(parse_result) => {
                    if let Some(episode_number) = parse_result.episode {
                        let (new_filename, new_path) =
                            generate_rename_path(bangumi, episode_number, file);

                        // Check if already renamed
                        if file.name.ends_with(&new_filename) {
                            tracing::debug!("File already renamed: {}", file.name);
                            results.push(RenameResult::AlreadyRenamed);
                            rename_count += 1; // Count as success
                            continue;
                        }

                        // Rename the file
                        match downloader
                            .rename_file(&torrent.info_hash, &file.name, &new_path)
                            .await
                        {
                            Ok(()) => {
                                tracing::info!("Renamed: {} -> {}", file.name, new_path);
                                rename_count += 1;
                                results.push(RenameResult::Renamed {
                                    old_path: file.name.clone(),
                                    new_path,
                                });
                            }
                            Err(e) => {
                                tracing::error!("Failed to rename {}: {}", file.name, e);
                                results.push(RenameResult::Failed {
                                    path: file.name.clone(),
                                    error: e.to_string(),
                                });
                            }
                        }
                    } else {
                        tracing::warn!("Could not extract episode number from: {}", filename);
                        results.push(RenameResult::ParseFailed {
                            path: file.name.clone(),
                            reason: "No episode number found".to_string(),
                        });
                    }
                }
                Err(e) => {
                    tracing::warn!("Failed to parse filename {}: {}", filename, e);
                    results.push(RenameResult::ParseFailed {
                        path: file.name.clone(),
                        reason: e.to_string(),
                    });
                }
            }
        }

        // Determine task status based on success rate
        let success_rate = rename_count as f64 / total_files as f64;

        if success_rate >= 0.5 {
            // Mark as completed if at least 50% of files were processed successfully
            DownloadTaskRepository::mark_completed(db, task.id).await?;
            tracing::info!(
                "Collection task {} completed: {} of {} files processed ({:.0}%)",
                task.id,
                rename_count,
                total_files,
                success_rate * 100.0
            );
        } else if rename_count > 0 {
            // Partial success - mark as completed but log warning
            DownloadTaskRepository::mark_completed(db, task.id).await?;
            tracing::warn!(
                "Collection task {} partially completed: {} of {} files processed ({:.0}%)",
                task.id,
                rename_count,
                total_files,
                success_rate * 100.0
            );
        } else {
            // Complete failure
            let error_msg = format!(
                "Failed to rename any files in collection: 0 of {} succeeded",
                total_files
            );
            DownloadTaskRepository::mark_failed(db, task.id, &error_msg).await?;
            return Err(FileRenameError::CollectionRenameFailed(error_msg));
        }

        Ok(results)
    }
}
