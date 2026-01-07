//! Log Cleanup Actor
//!
//! Periodically cleans up old logs from the database.

use std::sync::Arc;
use std::time::Duration;

use domain::services::LogService;

use super::actor::{spawn_periodic_actor, ActorHandle, PeriodicActor};

/// Cleanup interval (24 hours)
const CLEANUP_INTERVAL: Duration = Duration::from_secs(86400);

/// Default retention period in days
const DEFAULT_RETENTION_DAYS: i64 = 30;

/// Handle for communicating with LogCleanupActor
pub type LogCleanupHandle = ActorHandle;

/// Log Cleanup Actor
///
/// Runs a background task that cleans up old logs at regular intervals.
struct LogCleanupActor {
    logs: Arc<LogService>,
    retention_days: i64,
}

impl PeriodicActor for LogCleanupActor {
    fn interval(&self) -> Duration {
        CLEANUP_INTERVAL
    }

    fn name(&self) -> &'static str {
        "log_cleanup"
    }

    async fn execute(&mut self) {
        tracing::debug!("Log cleanup job started");

        match self.logs.cleanup(self.retention_days).await {
            Ok(deleted) => {
                if deleted > 0 {
                    tracing::info!("Log cleanup completed: {} logs deleted", deleted);
                } else {
                    tracing::debug!("Log cleanup completed: no old logs to delete");
                }
            }
            Err(e) => {
                tracing::error!("Log cleanup failed: {}", e);
            }
        }
    }
}

/// Create and start the log cleanup actor with default retention (30 days)
pub fn create_log_cleanup_actor(logs: Arc<LogService>) -> LogCleanupHandle {
    create_log_cleanup_actor_with_retention(logs, DEFAULT_RETENTION_DAYS)
}

/// Create and start the log cleanup actor with custom retention period
pub fn create_log_cleanup_actor_with_retention(
    logs: Arc<LogService>,
    retention_days: i64,
) -> LogCleanupHandle {
    spawn_periodic_actor(LogCleanupActor {
        logs,
        retention_days,
    })
}
