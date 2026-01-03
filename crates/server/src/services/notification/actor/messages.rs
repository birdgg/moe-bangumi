use notify::worker::Topic;

/// 通知 Actor 消息类型
pub enum NotificationMessage {
    /// 发送通知 (fire-and-forget)
    Notify {
        topic: Topic,
        title: String,
        content: String,
    },

    /// 发送带图片的通知 (fire-and-forget)
    NotifyWithPhoto {
        title: String,
        content: String,
        photo: Vec<u8>,
        cache_key: Option<String>,
    },

    /// 配置变化，重新初始化 notifier
    InvalidateConfig,
}
