use thiserror::Error;

/// Notification service errors
#[derive(Debug, Error)]
pub enum NotificationError {
    /// Provider is not configured
    #[error("Provider not configured: {0}")]
    NotConfigured(String),

    /// Worker error
    #[error("Worker error: {0}")]
    Worker(#[from] anyhow::Error),
}
