use serde::{Deserialize, Serialize};
use std::str::FromStr;
use utoipa::ToSchema;

use super::Clearable;

/// Torrent kind: single episode or collection (batch/season pack)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, ToSchema)]
#[serde(rename_all = "lowercase")]
pub enum TorrentKind {
    /// Single episode torrent
    Episode,
    /// Collection torrent (multiple episodes in one torrent)
    Collection,
}

impl TorrentKind {
    pub fn as_str(&self) -> &'static str {
        match self {
            TorrentKind::Episode => "episode",
            TorrentKind::Collection => "collection",
        }
    }

    pub fn is_collection(&self) -> bool {
        matches!(self, TorrentKind::Collection)
    }
}

impl FromStr for TorrentKind {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_lowercase().as_str() {
            "episode" => Ok(TorrentKind::Episode),
            "collection" => Ok(TorrentKind::Collection),
            _ => Err(format!("Invalid torrent kind: {}", s)),
        }
    }
}

impl Default for TorrentKind {
    fn default() -> Self {
        TorrentKind::Episode
    }
}

/// Torrent entity representing a BitTorrent file for bangumi episodes
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

    /// Torrent kind (episode or collection)
    pub kind: TorrentKind,

    /// Episode number (required for episode kind, None for collection)
    pub episode_number: Option<i32>,
}

impl Torrent {
    /// Check if this torrent is a collection
    pub fn is_collection(&self) -> bool {
        self.kind.is_collection()
    }
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

    /// Torrent kind (default: episode)
    #[serde(default)]
    pub kind: TorrentKind,

    /// Episode number (required for episode kind)
    pub episode_number: Option<i32>,
}

/// Request body for updating a torrent
#[derive(Debug, Clone, Default, Deserialize)]
pub struct UpdateTorrent {
    #[serde(default)]
    pub rss_id: Clearable<i64>,
    #[serde(default)]
    pub kind: Option<TorrentKind>,
    #[serde(default)]
    pub episode_number: Clearable<i32>,
}
