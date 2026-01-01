use std::collections::HashMap;
use std::sync::Arc;

use bgmtv::BgmtvClient;
use chrono::Datelike;
use futures::stream::{self, StreamExt};
use mikan::{MikanClient, Season};
use sqlx::SqlitePool;
use thiserror::Error;

use crate::models::{CreateMetadata, Platform};
use crate::repositories::{CalendarEntry, CalendarRepository, MetadataRepository};

#[derive(Debug, Error)]
pub enum CalendarError {
    #[error("Database error: {0}")]
    Database(#[from] sqlx::Error),

    #[error("BGM.tv API error: {0}")]
    Bgmtv(#[from] bgmtv::BgmtvError),

    #[error("Mikan API error: {0}")]
    Mikan(#[from] mikan::MikanError),
}

pub type Result<T> = std::result::Result<T, CalendarError>;

/// Calendar service for managing seasonal anime schedule
/// Data flow: Mikan Season API → BGM.tv Subject API → Metadata + Calendar tables
pub struct CalendarService {
    db: SqlitePool,
    bgmtv: Arc<BgmtvClient>,
    mikan: Arc<MikanClient>,
}

impl CalendarService {
    pub fn new(db: SqlitePool, bgmtv: Arc<BgmtvClient>, mikan: Arc<MikanClient>) -> Self {
        Self { db, bgmtv, mikan }
    }

    /// Get current year and season
    pub fn current_season() -> (i32, Season) {
        let now = chrono::Utc::now();
        let year = now.year();
        let season = Season::current();
        (year, season)
    }

    /// Refresh calendar data for a specific season
    /// 1. Fetch seasonal bangumi list from Mikan
    /// 2. Get BGM.tv ID for each bangumi
    /// 3. Fetch full metadata from BGM.tv
    /// 4. Upsert to metadata and calendar tables
    pub async fn refresh_calendar(&self, year: i32, season: Season) -> Result<usize> {
        let season_str = season.to_db_string();
        tracing::info!(
            "Refreshing calendar for {} {}",
            year,
            season_str
        );

        // Step 1: Get seasonal bangumi list from Mikan
        let seasonal_list = self.mikan.get_seasonal_bangumi_list(year, season).await?;
        tracing::info!(
            "Fetched {} bangumi from Mikan for {} {}",
            seasonal_list.len(),
            year,
            season_str
        );

        if seasonal_list.is_empty() {
            return Ok(0);
        }

        // Step 2: Concurrently fetch BGM.tv IDs (with rate limiting)
        let bgmtv_mappings = self.fetch_bgmtv_ids(&seasonal_list).await;
        tracing::info!(
            "Got {} BGM.tv IDs out of {} bangumi",
            bgmtv_mappings.len(),
            seasonal_list.len()
        );

        if bgmtv_mappings.is_empty() {
            return Ok(0);
        }

        // Step 3: Concurrently fetch BGM.tv subject details
        let subjects = self.fetch_subjects(&bgmtv_mappings).await;
        tracing::info!("Fetched {} subject details from BGM.tv", subjects.len());

        if subjects.is_empty() {
            return Ok(0);
        }

        // Step 4: Ensure metadata exists and get calendar entries
        let calendar_entries = self.ensure_metadata_and_build_entries(&subjects, year, &season_str).await?;
        tracing::info!("Built {} calendar entries", calendar_entries.len());

        // Step 5: Batch upsert to calendar table
        CalendarRepository::upsert_batch(&self.db, &calendar_entries).await?;

        // Step 6: Clean up stale entries
        let keep_metadata_ids: Vec<i64> = calendar_entries.iter().map(|e| e.metadata_id).collect();
        let deleted = CalendarRepository::delete_except(&self.db, year, &season_str, &keep_metadata_ids).await?;
        if deleted > 0 {
            tracing::info!("Removed {} stale calendar entries", deleted);
        }

        tracing::info!(
            "Successfully refreshed {} calendar entries for {} {}",
            calendar_entries.len(),
            year,
            season_str
        );

        Ok(calendar_entries.len())
    }

    /// Get calendar data for a specific season
    pub async fn get_calendar(&self, year: i32, season: Season) -> Result<Vec<bgmtv::CalendarDay>> {
        let season_str = season.to_db_string();
        let entries = CalendarRepository::get_by_season(&self.db, year, &season_str).await?;

        // Group by weekday (1-7)
        let mut weekday_map: HashMap<i32, Vec<bgmtv::CalendarSubject>> = HashMap::new();

        for entry in entries {
            let subject = entry.to_calendar_subject();
            let weekday = entry.air_weekday();
            weekday_map.entry(weekday).or_default().push(subject);
        }

        // Convert to CalendarDay format
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

    /// Check if calendar data exists for a season
    pub async fn has_data(&self, year: i32, season: Season) -> Result<bool> {
        let season_str = season.to_db_string();
        Ok(CalendarRepository::has_data(&self.db, year, &season_str).await?)
    }

    /// Concurrently fetch BGM.tv IDs for bangumi list
    /// Returns (mikan_id, bgmtv_id, air_week) tuples, skipping failures
    async fn fetch_bgmtv_ids(
        &self,
        bangumi_list: &[mikan::SeasonalBangumi],
    ) -> Vec<(String, i64, i32)> {
        // Clone data upfront to avoid lifetime issues
        let items: Vec<_> = bangumi_list
            .iter()
            .map(|b| (b.mikan_id.clone(), b.air_week, Arc::clone(&self.mikan)))
            .collect();

        let tasks = items.into_iter().map(|(mikan_id, air_week, mikan)| async move {
            match mikan.get_bangumi_bgmtv_id(&mikan_id).await {
                Ok(Some(bgmtv_id)) => Some((mikan_id, bgmtv_id, air_week)),
                Ok(None) => {
                    tracing::debug!("No BGM.tv ID found for mikan_id: {}", mikan_id);
                    None
                }
                Err(e) => {
                    tracing::warn!("Failed to fetch BGM.tv ID for {}: {}", mikan_id, e);
                    None
                }
            }
        });

        // Limit concurrency to avoid rate limiting
        stream::iter(tasks)
            .buffer_unordered(5)
            .filter_map(|x| async { x })
            .collect()
            .await
    }

    /// Concurrently fetch BGM.tv subject details
    /// Returns (mikan_id, bgmtv_id, air_week, SubjectDetail) tuples, skipping failures
    async fn fetch_subjects(
        &self,
        mappings: &[(String, i64, i32)],
    ) -> Vec<(String, i64, i32, bgmtv::SubjectDetail)> {
        // Clone data upfront to avoid lifetime issues
        let items: Vec<_> = mappings
            .iter()
            .map(|(mikan_id, bgmtv_id, air_week)| (mikan_id.clone(), *bgmtv_id, *air_week, Arc::clone(&self.bgmtv)))
            .collect();

        let tasks = items.into_iter().map(|(mikan_id, bgmtv_id, air_week, bgmtv)| async move {
            match bgmtv.get_subject(bgmtv_id).await {
                Ok(subject) => Some((mikan_id, bgmtv_id, air_week, subject)),
                Err(e) => {
                    tracing::warn!(
                        "Failed to fetch BGM.tv subject {}: {}",
                        bgmtv_id,
                        e
                    );
                    None
                }
            }
        });

        // Limit concurrency
        stream::iter(tasks)
            .buffer_unordered(10)
            .filter_map(|x| async { x })
            .collect()
            .await
    }

    /// Ensure metadata exists for each subject and build calendar entries
    async fn ensure_metadata_and_build_entries(
        &self,
        subjects: &[(String, i64, i32, bgmtv::SubjectDetail)],
        year: i32,
        season_str: &str,
    ) -> Result<Vec<CalendarEntry>> {
        let mut entries = Vec::new();

        for (mikan_id, bgmtv_id, air_week, subject) in subjects {
            // Check if metadata already exists
            let existing = MetadataRepository::get_by_bgmtv_id(&self.db, *bgmtv_id).await?;

            let metadata_id = if let Some(metadata) = existing {
                metadata.id
            } else {
                // Create new metadata (use air_week from Mikan)
                let create_data = self.build_create_metadata(mikan_id, *bgmtv_id, *air_week, subject);
                let metadata = MetadataRepository::create(&self.db, create_data).await?;
                metadata.id
            };

            // Priority defaults to 0 (SubjectDetail doesn't have collection info)
            let priority = 0;

            entries.push(CalendarEntry {
                metadata_id,
                year,
                season: season_str.to_string(),
                priority,
            });
        }

        Ok(entries)
    }

    /// Build CreateMetadata from BGM.tv SubjectDetail
    fn build_create_metadata(
        &self,
        mikan_id: &str,
        bgmtv_id: i64,
        air_week: i32,
        subject: &bgmtv::SubjectDetail,
    ) -> CreateMetadata {
        // Parse year from date
        let year = subject
            .date
            .as_ref()
            .and_then(|date| date.get(0..4))
            .and_then(|s| s.parse().ok())
            .unwrap_or_else(|| chrono::Utc::now().year());

        // Parse platform
        let platform = subject
            .platform
            .as_ref()
            .map(|p| match p.to_lowercase().as_str() {
                "movie" | "劇場版" => Platform::Movie,
                "ova" => Platform::Ova,
                _ => Platform::Tv,
            })
            .unwrap_or(Platform::Tv);

        CreateMetadata {
            mikan_id: Some(mikan_id.to_string()),
            bgmtv_id: Some(bgmtv_id),
            tmdb_id: None,
            title_chinese: if subject.name_cn.is_empty() {
                subject.name.clone()
            } else {
                subject.name_cn.clone()
            },
            title_japanese: Some(subject.name.clone()).filter(|s| !s.is_empty()),
            season: 1, // Default to season 1
            year,
            platform,
            total_episodes: subject.total_episodes as i32,
            poster_url: Some(subject.images.large.clone()),
            air_date: subject.date.clone(),
            air_week,
            finished: false,
        }
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
