use serde::{Deserialize, Serialize};
use std::str::FromStr;
use utoipa::ToSchema;

use super::Clearable;

/// Download task status
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Serialize, Deserialize, ToSchema)]
#[serde(rename_all = "lowercase")]
pub enum DownloadTaskStatus {
    #[default]
    Pending,
    Paused,
    Downloading,
    Completed,
    Failed,
}

impl DownloadTaskStatus {
    pub fn as_str(&self) -> &'static str {
        match self {
            DownloadTaskStatus::Pending => "pending",
            DownloadTaskStatus::Paused => "paused",
            DownloadTaskStatus::Downloading => "downloading",
            DownloadTaskStatus::Completed => "completed",
            DownloadTaskStatus::Failed => "failed",
        }
    }
}

impl FromStr for DownloadTaskStatus {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_lowercase().as_str() {
            "pending" => Ok(DownloadTaskStatus::Pending),
            "paused" => Ok(DownloadTaskStatus::Paused),
            "downloading" => Ok(DownloadTaskStatus::Downloading),
            "completed" => Ok(DownloadTaskStatus::Completed),
            "failed" => Ok(DownloadTaskStatus::Failed),
            _ => Err(format!("Invalid download task status: {}", s)),
        }
    }
}

/// Download task entity tracking download progress and history
#[derive(Debug, Clone, Serialize, Deserialize, ToSchema)]
pub struct DownloadTask {
    pub id: i64,
    pub created_at: chrono::DateTime<chrono::Utc>,
    pub updated_at: chrono::DateTime<chrono::Utc>,

    /// Foreign key to torrent
    pub torrent_id: i64,
    /// Task status
    pub status: DownloadTaskStatus,
    /// Error message for failed tasks
    pub error_message: Option<String>,
}

/// Request body for creating a new download task
#[derive(Debug, Clone, Serialize, Deserialize, ToSchema)]
pub struct CreateDownloadTask {
    /// Foreign key to torrent
    pub torrent_id: i64,
}

/// Request body for updating a download task
#[derive(Debug, Clone, Default, Deserialize)]
pub struct UpdateDownloadTask {
    #[serde(default)]
    pub status: Option<DownloadTaskStatus>,
    #[serde(default)]
    pub error_message: Clearable<String>,
}
