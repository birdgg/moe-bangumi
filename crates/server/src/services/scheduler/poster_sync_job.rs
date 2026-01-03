use async_trait::async_trait;
use sqlx::SqlitePool;
use std::sync::Arc;
use std::time::Duration;

use super::traits::{JobResult, SchedulerJob};
use crate::repositories::MetadataRepository;
use crate::services::PosterService;

/// Poster sync job that downloads remote poster images to local storage.
///
/// This job runs every 24 hours and:
/// 1. Scans the metadata table for records with remote poster URLs
/// 2. Downloads each poster to local storage
/// 3. Updates the database with the local path
pub struct PosterSyncJob {
    db: SqlitePool,
    poster: Arc<PosterService>,
}

impl PosterSyncJob {
    /// Creates a new poster sync job.
    pub fn new(db: SqlitePool, poster: Arc<PosterService>) -> Self {
        Self { db, poster }
    }
}

#[async_trait]
impl SchedulerJob for PosterSyncJob {
    fn name(&self) -> &'static str {
        "PosterSync"
    }

    fn interval(&self) -> Duration {
        Duration::from_secs(86400) // Every 24 hours
    }

    async fn execute(&self) -> JobResult {
        tracing::info!("PosterSync: Starting poster sync job");

        // Get all metadata with remote poster URLs
        let remote_posters = MetadataRepository::get_remote_poster_metadata(&self.db).await?;

        if remote_posters.is_empty() {
            tracing::info!("PosterSync: No remote poster URLs found");
            return Ok(());
        }

        tracing::info!(
            "PosterSync: Found {} metadata records with remote poster URLs",
            remote_posters.len()
        );

        let mut succeeded = 0;
        let mut failed = 0;

        for (metadata_id, poster_url) in remote_posters {
            match self.poster.download_from_url(&poster_url).await {
                Ok(local_path) => {
                    // Update database with local path
                    match MetadataRepository::update_poster_url(&self.db, metadata_id, &local_path)
                        .await
                    {
                        Ok(true) => {
                            tracing::info!(
                                "PosterSync: Successfully synced poster for metadata {}: {} -> {}",
                                metadata_id,
                                poster_url,
                                local_path
                            );
                            succeeded += 1;
                        }
                        Ok(false) => {
                            tracing::warn!(
                                "PosterSync: Metadata {} not found when updating poster",
                                metadata_id
                            );
                            failed += 1;
                        }
                        Err(e) => {
                            tracing::error!(
                                "PosterSync: Failed to update poster URL for metadata {}: {}",
                                metadata_id,
                                e
                            );
                            failed += 1;
                        }
                    }
                }
                Err(e) => {
                    tracing::warn!(
                        "PosterSync: Failed to download poster for metadata {}: {}",
                        metadata_id,
                        e
                    );
                    failed += 1;
                }
            }
        }

        tracing::info!(
            "PosterSync: Poster sync job completed: {} succeeded, {} failed",
            succeeded,
            failed
        );

        Ok(())
    }
}
