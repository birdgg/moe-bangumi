//! Tracked task processing
//!
//! Handles renaming of torrents that exist in the database with
//! associated bangumi metadata.

use parser::Parser;

use crate::models::{Bangumi, Torrent};
use crate::services::{DownloaderHandle, Task};
use crate::utils::is_temp_download_path;

use super::bdrip::BDRipProcessor;
use super::standard::StandardProcessor;
use super::types::{RenameTaskResult, Result};
use super::RenameError;

/// Tracked task processor
///
/// Processes torrents that are tracked in the database and have
/// associated bangumi metadata.
pub(super) struct TrackedProcessor;

impl TrackedProcessor {
    /// Process a tracked task (torrent exists in database)
    ///
    /// Returns the result containing bangumi info and successfully renamed episodes.
    pub(super) async fn process(
        downloader: &DownloaderHandle,
        parser: &Parser,
        task: &Task,
        torrent: &Torrent,
        bangumi: &Bangumi,
    ) -> Result<RenameTaskResult> {
        tracing::info!(
            "Processing task: {} ({}) for bangumi: {}",
            task.name,
            task.id,
            bangumi.title_chinese
        );

        // Check if task is in temporary directory and move to final location first
        if is_temp_download_path(&task.save_path) {
            let final_path = &bangumi.save_path;
            tracing::info!(
                "Moving task from temporary location {} to final location {}",
                task.save_path,
                final_path
            );

            match downloader.set_location(&task.id, final_path).await {
                Ok(()) => {
                    tracing::info!("Successfully moved task {} to {}", task.id, final_path);
                }
                Err(e) => {
                    tracing::error!(
                        "Failed to move task {} to final location: {}. Will retry on next rename cycle.",
                        task.id,
                        e
                    );
                    // Return error to skip this task - it will be retried next cycle
                    // since the rename tag hasn't been removed
                    return Err(RenameError::Downloader(e));
                }
            }
        }

        // Get file list from downloader
        let files = downloader.get_task_files(&task.id).await?;

        // Filter to video files only
        let video_files: Vec<_> = files.iter().filter(|f| f.is_video()).collect();

        if video_files.is_empty() {
            tracing::warn!("No video files found in task: {}", task.name);
            // Return empty result - caller will handle finalization
            return Ok(RenameTaskResult {
                bangumi_id: bangumi.id,
                bangumi_title: bangumi.title_chinese.clone(),
                poster_url: bangumi.poster_url.clone(),
                total_episodes: bangumi.total_episodes,
                renamed_episodes: Vec::new(),
            });
        }

        // Check if this is a BDRip task
        let is_bdrip = BDRipProcessor::is_bdrip(task, bangumi, &files);

        let renamed_episodes = if is_bdrip {
            tracing::info!("Detected BDRip structure for task: {}", task.name);
            BDRipProcessor::process(downloader, task, &video_files, bangumi, &files).await?
        } else {
            StandardProcessor::process(
                downloader,
                parser,
                task,
                &video_files,
                torrent,
                bangumi,
                &files,
            )
            .await?
        };

        tracing::info!("Successfully renamed task: {}", task.name);
        Ok(RenameTaskResult {
            bangumi_id: bangumi.id,
            bangumi_title: bangumi.title_chinese.clone(),
            poster_url: bangumi.poster_url.clone(),
            total_episodes: bangumi.total_episodes,
            renamed_episodes,
        })
    }
}
