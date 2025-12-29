use reqwest::Client;
use serde_json::json;

use crate::TelegramError;

/// Telegram Bot API client
pub struct TelegramClient {
    client: Client,
    bot_token: String,
    chat_id: String,
}

impl TelegramClient {
    /// Create a new Telegram client
    pub fn new(client: Client, bot_token: impl Into<String>, chat_id: impl Into<String>) -> Self {
        Self {
            client,
            bot_token: bot_token.into(),
            chat_id: chat_id.into(),
        }
    }

    /// Send a text message
    pub async fn send_message(&self, text: &str) -> Result<(), TelegramError> {
        let url = format!(
            "https://api.telegram.org/bot{}/sendMessage",
            self.bot_token
        );

        let resp = self
            .client
            .post(&url)
            .json(&json!({
                "chat_id": self.chat_id,
                "text": text,
                "parse_mode": "HTML"
            }))
            .send()
            .await?;

        if !resp.status().is_success() {
            let error_text = resp.text().await.unwrap_or_default();
            return Err(TelegramError::Api(error_text));
        }

        Ok(())
    }

    /// Send a photo with caption
    pub async fn send_photo(&self, photo_url: &str, caption: &str) -> Result<(), TelegramError> {
        let url = format!(
            "https://api.telegram.org/bot{}/sendPhoto",
            self.bot_token
        );

        let resp = self
            .client
            .post(&url)
            .json(&json!({
                "chat_id": self.chat_id,
                "photo": photo_url,
                "caption": caption,
                "parse_mode": "HTML"
            }))
            .send()
            .await?;

        if !resp.status().is_success() {
            // Fallback: try sending as text only
            tracing::warn!("Failed to send photo, falling back to text message");
            return self.send_message(caption).await;
        }

        Ok(())
    }
}
