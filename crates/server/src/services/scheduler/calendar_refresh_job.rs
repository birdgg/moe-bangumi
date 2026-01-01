use async_trait::async_trait;
use std::sync::Arc;
use std::time::Duration;

use super::traits::{JobResult, SchedulerJob};
use crate::services::CalendarService;

/// Calendar refresh job that runs daily.
///
/// This job fetches the latest calendar data from Mikan/BGM.tv and updates the database.
/// Runs every 24 hours to keep the weekly schedule fresh.
pub struct CalendarRefreshJob {
    calendar_service: Arc<CalendarService>,
}

impl CalendarRefreshJob {
    /// Creates a new calendar refresh job.
    pub fn new(calendar_service: Arc<CalendarService>) -> Self {
        Self { calendar_service }
    }
}

#[async_trait]
impl SchedulerJob for CalendarRefreshJob {
    fn name(&self) -> &'static str {
        "CalendarRefresh"
    }

    fn interval(&self) -> Duration {
        // Run every 24 hours
        Duration::from_secs(24 * 3600)
    }

    async fn execute(&self) -> JobResult {
        let (year, season) = CalendarService::current_season();
        tracing::info!("Starting calendar refresh job for {} {:?}", year, season);

        match self.calendar_service.refresh_calendar(year, season).await {
            Ok(count) => {
                tracing::info!("Calendar refresh completed: {} entries updated", count);
                Ok(())
            }
            Err(e) => {
                tracing::error!("Calendar refresh failed: {}", e);
                Err(Box::new(e))
            }
        }
    }
}
