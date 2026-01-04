mod actor;
mod config;
mod handle;
pub mod telegram;

use anyhow::Result;
use async_trait::async_trait;
use tokio::sync::mpsc;

pub use config::{NotificationConfig, TelegramConfig};
pub use handle::NotificationHandle;

/// 通知主题
#[derive(Debug, Clone, Copy, Hash, Eq, PartialEq)]
pub enum Topic {
    Download,
    System,
    Error,
}

impl std::fmt::Display for Topic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Topic::Download => write!(f, "下载"),
            Topic::System => write!(f, "系统"),
            Topic::Error => write!(f, "错误"),
        }
    }
}

/// 通知系统核心 trait
#[async_trait]
pub trait Notifier: Send + Sync {
    /// 发送文本消息
    async fn send_message(&self, text: &str) -> Result<()>;

    /// 发送带格式的消息（Markdown/HTML等）
    async fn send_formatted_message(&self, text: &str, parse_mode: &str) -> Result<()>;

    /// 发送带附件的消息
    async fn send_message_with_attachment(
        &self,
        text: &str,
        attachment: &[u8],
        file_name: &str,
    ) -> Result<()>;

    /// 发送带图片的消息
    ///
    /// `cache_key` is used to cache the file_id for reuse (e.g., poster path).
    /// If the same cache_key is used again, the cached file_id will be used
    /// instead of re-uploading the photo.
    async fn send_photo(
        &self,
        caption: &str,
        photo: &[u8],
        parse_mode: &str,
        cache_key: Option<&str>,
    ) -> Result<()>;
}

/// 创建通知服务
pub fn create_notification_service(
    config: NotificationConfig,
    http_client: reqwest::Client,
) -> NotificationHandle {
    let (sender, receiver) = mpsc::channel(32);

    let actor = actor::NotificationActor::new(config, http_client, receiver);
    tokio::spawn(actor.run());

    NotificationHandle::new(sender)
}
