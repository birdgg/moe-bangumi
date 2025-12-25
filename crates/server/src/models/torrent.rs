use serde::{Deserialize, Serialize};
use utoipa::ToSchema;

use super::Clearable;

/// Torrent entity representing a BitTorrent file for a bangumi episode
#[derive(Debug, Clone, Serialize, Deserialize, ToSchema)]
pub struct Torrent {
    pub id: i64,
    pub created_at: chrono::DateTime<chrono::Utc>,
    pub updated_at: chrono::DateTime<chrono::Utc>,

    /// Foreign key to bangumi
    pub bangumi_id: i64,
    /// Optional reference to source RSS
    pub rss_id: Option<i64>,

    /// BitTorrent info hash (40-char hex for v1, 64-char for v2)
    pub info_hash: String,
    /// Episode number this torrent represents
    pub episode_number: i32,
}

/// Request body for creating a new torrent
#[derive(Debug, Clone, Serialize, Deserialize, ToSchema)]
pub struct CreateTorrent {
    /// Foreign key to bangumi
    pub bangumi_id: i64,
    /// Optional reference to source RSS
    pub rss_id: Option<i64>,
    /// BitTorrent info hash
    pub info_hash: String,
    /// Episode number
    pub episode_number: i32,
}

/// Request body for updating a torrent
#[derive(Debug, Clone, Default, Deserialize)]
pub struct UpdateTorrent {
    #[serde(default)]
    pub rss_id: Clearable<i64>,
    #[serde(default)]
    pub episode_number: Option<i32>,
}
