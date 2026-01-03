use thiserror::Error;

#[derive(Debug, Error)]
pub enum DownloaderError {
    #[error("HTTP request failed: {0}")]
    Request(#[from] reqwest::Error),

    #[error("qBittorrent error: {0}")]
    QBittorrent(#[from] qbittorrent::QBittorrentError),

    #[error("Transmission error: {0}")]
    Transmission(String),

    #[error("Configuration error: {0}")]
    Config(String),

    #[error("Authentication failed: {0}")]
    Auth(String),

    #[error("Downloader not configured")]
    NotConfigured,

    #[error("Downloader service unavailable (actor stopped)")]
    ServiceUnavailable,

    #[error("Operation not supported: {0}")]
    NotSupported(String),
}

impl DownloaderError {
    /// 判断是否为认证错误
    pub fn is_auth_error(&self) -> bool {
        match self {
            DownloaderError::Auth(_) => true,
            DownloaderError::QBittorrent(qb_err) => match qb_err {
                qbittorrent::QBittorrentError::Auth(_) => true,
                qbittorrent::QBittorrentError::Api { status_code, .. } => {
                    *status_code == 401 || *status_code == 403
                }
                _ => false,
            },
            _ => false,
        }
    }
}

pub type Result<T> = std::result::Result<T, DownloaderError>;
