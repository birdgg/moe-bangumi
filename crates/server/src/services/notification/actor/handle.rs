use notify::worker::Topic;
use tokio::sync::mpsc;

use super::messages::NotificationMessage;

/// NotificationService 的对外接口（Handle）
///
/// 这个结构体提供与原 NotificationService 兼容的 API，
/// 内部通过 channel 与 Actor 通信。
#[derive(Clone)]
pub struct NotificationHandle {
    sender: mpsc::Sender<NotificationMessage>,
}

impl NotificationHandle {
    pub fn new(sender: mpsc::Sender<NotificationMessage>) -> Self {
        Self { sender }
    }

    /// 发送通知 (fire-and-forget)
    pub fn notify(&self, topic: Topic, title: impl Into<String>, content: impl Into<String>) {
        let sender = self.sender.clone();
        let title = title.into();
        let content = content.into();
        tokio::spawn(async move {
            let _ = sender
                .send(NotificationMessage::Notify {
                    topic,
                    title,
                    content,
                })
                .await;
        });
    }

    /// 发送错误通知 (fire-and-forget)
    pub fn notify_error(&self, title: impl Into<String>, error: impl std::fmt::Display) {
        self.notify(Topic::Error, title, format!("错误信息: {}", error));
    }

    /// 发送下载通知 (fire-and-forget)
    pub fn notify_download(&self, title: impl Into<String>, content: impl Into<String>) {
        self.notify(Topic::Download, title, content);
    }

    /// 发送带图片的下载通知 (fire-and-forget)
    pub fn notify_download_with_photo(
        &self,
        title: impl Into<String>,
        content: impl Into<String>,
        photo: Vec<u8>,
        cache_key: Option<String>,
    ) {
        let sender = self.sender.clone();
        let title = title.into();
        let content = content.into();
        tokio::spawn(async move {
            let _ = sender
                .send(NotificationMessage::NotifyWithPhoto {
                    title,
                    content,
                    photo,
                    cache_key,
                })
                .await;
        });
    }

    /// 发送失效通知（内部使用）
    pub(crate) async fn invalidate(&self) {
        let _ = self.sender.send(NotificationMessage::InvalidateConfig).await;
    }
}
