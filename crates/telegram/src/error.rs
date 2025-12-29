use thiserror::Error;

/// Telegram API errors
#[derive(Debug, Error)]
pub enum TelegramError {
    /// HTTP request failed
    #[error("HTTP request failed: {0}")]
    Http(#[from] reqwest::Error),

    /// Telegram API returned an error
    #[error("Telegram API error: {0}")]
    Api(String),
}
