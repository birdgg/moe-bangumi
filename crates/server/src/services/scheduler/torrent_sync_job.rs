use async_trait::async_trait;
use sqlx::SqlitePool;
use std::collections::HashSet;
use std::sync::Arc;
use std::time::Duration;

use super::traits::{JobResult, SchedulerJob};
use crate::repositories::TorrentRepository;
use crate::services::{AddTaskOptions, DownloaderService, TaskFilter};

/// Torrent sync job that runs every 10 minutes.
///
/// This job ensures database records and downloader tasks are in sync.
/// If a torrent exists in the database but not in the downloader
/// (e.g., due to a failed add_task call), it will be re-added.
pub struct TorrentSyncJob {
    db: SqlitePool,
    downloader: Arc<DownloaderService>,
}

impl TorrentSyncJob {
    /// Creates a new torrent sync job.
    pub fn new(db: SqlitePool, downloader: Arc<DownloaderService>) -> Self {
        Self { db, downloader }
    }
}

#[async_trait]
impl SchedulerJob for TorrentSyncJob {
    fn name(&self) -> &'static str {
        "TorrentSync"
    }

    fn interval(&self) -> Duration {
        Duration::from_secs(600) // Every 10 minutes
    }

    async fn execute(&self) -> JobResult {
        // 1. Check if downloader is available
        if !self.downloader.is_available().await {
            tracing::debug!("Downloader not available, skipping sync");
            return Ok(());
        }

        // 2. Get all torrents from database
        let db_torrents = TorrentRepository::get_all_for_sync(&self.db).await?;
        if db_torrents.is_empty() {
            return Ok(());
        }

        // 3. Get all tasks from downloader (filter by "moe" tag for efficiency)
        let filter = TaskFilter {
            tag: Some("moe".to_string()),
            ..Default::default()
        };
        let downloader_tasks = match self.downloader.get_tasks(Some(&filter)).await {
            Ok(tasks) => tasks,
            Err(e) => {
                tracing::warn!("Failed to get tasks from downloader: {}", e);
                return Ok(()); // Don't fail the job, just skip this cycle
            }
        };

        // 4. Build set of downloader task hashes for O(1) lookup
        let downloader_hashes: HashSet<String> =
            downloader_tasks.into_iter().map(|t| t.id).collect();

        // 5. Find missing torrents and re-add them
        let mut synced_count = 0;
        for torrent in db_torrents {
            if !downloader_hashes.contains(&torrent.info_hash) {
                tracing::info!(
                    "Torrent {} missing from downloader, re-adding",
                    torrent.info_hash
                );

                let options = AddTaskOptions::new(&torrent.torrent_url)
                    .save_path(&torrent.save_path)
                    .add_tag("moe")
                    .add_tag("rename");

                self.downloader.add_task(options);
                synced_count += 1;
            }
        }

        if synced_count > 0 {
            tracing::info!("TorrentSync: re-added {} missing torrents", synced_count);
        }

        Ok(())
    }
}
