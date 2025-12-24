use serde::{Deserialize, Serialize};
use utoipa::ToSchema;

/// Downloader type
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, ToSchema)]
#[serde(rename_all = "lowercase")]
pub enum DownloaderType {
    QBittorrent,
}
