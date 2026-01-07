//! Rename Actor
//!
//! Periodically scans for completed downloads and renames them to Plex/Jellyfin compatible names.

use std::sync::Arc;
use std::time::Duration;

use domain::services::RenameService;

use super::actor::{spawn_periodic_actor, ActorHandle, PeriodicActor};

/// Rename interval (10 minutes)
const RENAME_INTERVAL: Duration = Duration::from_secs(600);

/// Handle for communicating with RenameActor
pub type RenameHandle = ActorHandle;

/// Rename Actor
///
/// Runs a background task that processes file renames at regular intervals.
struct RenameActor {
    rename_service: Arc<RenameService>,
}

impl PeriodicActor for RenameActor {
    fn interval(&self) -> Duration {
        RENAME_INTERVAL
    }

    fn name(&self) -> &'static str {
        "rename"
    }

    async fn execute(&mut self) {
        tracing::debug!("Rename job started");

        if let Err(e) = self.rename_service.process_all().await {
            tracing::error!("Rename job failed: {}", e);
        } else {
            tracing::debug!("Rename job completed");
        }
    }
}

/// Create and start the rename actor
pub fn create_rename_actor(rename_service: Arc<RenameService>) -> RenameHandle {
    spawn_periodic_actor(RenameActor { rename_service })
}
