use bgmtv::BgmtvClient;
use sqlx::SqlitePool;
use std::collections::HashMap;
use std::sync::Arc;
use thiserror::Error;

use crate::repositories::{CalendarRepository, CalendarSubjectRow};

#[derive(Debug, Error)]
pub enum CalendarError {
    #[error("Database error: {0}")]
    Database(#[from] sqlx::Error),

    #[error("BGM.tv API error: {0}")]
    Bgmtv(#[from] bgmtv::BgmtvError),
}

pub type Result<T> = std::result::Result<T, CalendarError>;

/// Calendar service for managing BGM.tv calendar data
pub struct CalendarService {
    db: SqlitePool,
    bgmtv: Arc<BgmtvClient>,
}

impl CalendarService {
    pub fn new(db: SqlitePool, bgmtv: Arc<BgmtvClient>) -> Self {
        Self { db, bgmtv }
    }

    /// Fetch fresh calendar data from BGM.tv and update database
    /// This replaces all existing data with the latest from API
    pub async fn refresh_calendar(&self) -> Result<usize> {
        tracing::info!("Fetching calendar from BGM.tv API");

        // Fetch from BGM.tv API
        let calendar_days = self.bgmtv.get_calendar().await?;

        // Flatten all subjects from all days and convert to database rows
        let subjects: Vec<CalendarSubjectRow> = calendar_days
            .iter()
            .flat_map(|day| &day.items)
            .map(CalendarSubjectRow::from_bgmtv)
            .collect();

        let count = subjects.len();
        tracing::info!("Fetched {} calendar subjects from BGM.tv", count);

        // Collect all IDs for cleanup
        let keep_ids: Vec<i64> = subjects.iter().map(|s| s.bgmtv_id).collect();

        // Batch upsert to database
        CalendarRepository::upsert_batch(&self.db, &subjects).await?;

        // Delete any subjects that are no longer in the calendar
        let deleted = CalendarRepository::delete_except(&self.db, &keep_ids).await?;
        if deleted > 0 {
            tracing::info!("Removed {} stale calendar subjects", deleted);
        }

        tracing::info!(
            "Successfully updated {} calendar subjects in database",
            count
        );
        Ok(count)
    }

    /// Get calendar data from database, grouped by weekday
    /// Returns data in the same format as BGM.tv API
    pub async fn get_calendar(&self) -> Result<Vec<bgmtv::CalendarDay>> {
        let subjects = CalendarRepository::get_all(&self.db).await?;

        // Group by weekday (1-7)
        let mut weekday_map: HashMap<i32, Vec<bgmtv::CalendarSubject>> = HashMap::new();

        for subject in subjects {
            let bgmtv_subject = subject.to_bgmtv();
            weekday_map
                .entry(subject.air_weekday)
                .or_default()
                .push(bgmtv_subject);
        }

        // Convert to CalendarDay format, only include non-empty days
        let calendar_days: Vec<bgmtv::CalendarDay> = (1..=7)
            .filter_map(|weekday_id| {
                weekday_map.remove(&weekday_id).map(|items| bgmtv::CalendarDay {
                    weekday: Self::weekday_info(weekday_id),
                    items,
                })
            })
            .collect();

        Ok(calendar_days)
    }

    /// Get calendar subjects for a specific weekday
    pub async fn get_by_weekday(&self, weekday: i32) -> Result<Vec<bgmtv::CalendarSubject>> {
        let subjects = CalendarRepository::get_by_weekday(&self.db, weekday).await?;
        Ok(subjects.into_iter().map(|s| s.to_bgmtv()).collect())
    }

    /// Check if calendar data exists in database
    pub async fn has_data(&self) -> Result<bool> {
        Ok(CalendarRepository::has_data(&self.db).await?)
    }

    /// Helper: Create weekday info
    fn weekday_info(id: i32) -> bgmtv::Weekday {
        let (en, cn, ja) = match id {
            1 => ("Mon", "星期一", "月曜日"),
            2 => ("Tue", "星期二", "火曜日"),
            3 => ("Wed", "星期三", "水曜日"),
            4 => ("Thu", "星期四", "木曜日"),
            5 => ("Fri", "星期五", "金曜日"),
            6 => ("Sat", "星期六", "土曜日"),
            7 => ("Sun", "星期日", "日曜日"),
            _ => ("Unknown", "未知", "不明"),
        };

        bgmtv::Weekday {
            en: en.to_string(),
            cn: cn.to_string(),
            ja: ja.to_string(),
            id,
        }
    }
}
