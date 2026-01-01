use serde::{Deserialize, Serialize};

/// Calendar seed data structure for importing historical data
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CalendarSeedData {
    pub seasons: Vec<SeasonData>,
}

/// Data for a single season
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SeasonData {
    pub year: i32,
    pub season: String,
    pub entries: Vec<CalendarSeedEntry>,
}

/// Individual calendar entry in seed data
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CalendarSeedEntry {
    pub mikan_id: String,
    pub bgmtv_id: i64,
    pub title_chinese: String,
    pub title_japanese: Option<String>,
    pub air_week: i32,
    pub poster_url: Option<String>,
    pub year: i32,
    pub platform: String,
    pub total_episodes: i32,
    pub air_date: Option<String>,
}
