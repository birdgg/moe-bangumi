use anyhow::Result;
use async_trait::async_trait;
pub mod telegram;
pub mod worker;

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
