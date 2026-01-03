use std::sync::Arc;

use notify::worker::{Topic, Worker as NotificationWorker};
use tokio::sync::mpsc;

use super::messages::NotificationMessage;
use crate::models::TelegramConfig;
use crate::services::{HttpClientService, SettingsService};

/// 通知 Actor 主循环
pub struct NotificationActor {
    worker: NotificationWorker,
    receiver: mpsc::Receiver<NotificationMessage>,
    settings: Arc<SettingsService>,
    http_client: Arc<HttpClientService>,
}

impl NotificationActor {
    pub fn new(
        settings: Arc<SettingsService>,
        http_client: Arc<HttpClientService>,
        receiver: mpsc::Receiver<NotificationMessage>,
    ) -> Self {
        let mut worker = NotificationWorker::new();

        // Initialize with current settings
        let current_settings = settings.get();
        if let Some(notifier) = Self::create_telegram_notifier(
            &current_settings.notification.telegram,
            http_client.get_client(),
        ) {
            worker.add_notifier(notifier);
        }

        Self {
            worker,
            receiver,
            settings,
            http_client,
        }
    }

    fn create_telegram_notifier(
        config: &TelegramConfig,
        client: reqwest::Client,
    ) -> Option<Box<dyn notify::Notifier>> {
        if config.enabled && !config.bot_token.is_empty() && !config.chat_id.is_empty() {
            match notify::telegram::TelegramNotifier::new_with_client(
                client,
                &config.bot_token,
                &config.chat_id,
            ) {
                Ok(notifier) => Some(Box::new(notifier)),
                Err(e) => {
                    tracing::error!("Failed to create Telegram notifier: {}", e);
                    None
                }
            }
        } else {
            None
        }
    }

    /// 运行 Actor 主循环
    pub async fn run(mut self) {
        tracing::info!("Notification actor started");

        // Start the worker
        let settings = self.settings.get();
        if settings.notification.enabled {
            if let Err(e) = self.worker.spawn().await {
                tracing::error!("Failed to spawn notification worker: {}", e);
            }
        }

        while let Some(msg) = self.receiver.recv().await {
            self.handle_message(msg).await;
        }

        // Shutdown worker
        if let Err(e) = self.worker.shutdown().await {
            tracing::error!("Failed to shutdown notification worker: {}", e);
        }

        tracing::info!("Notification actor stopped");
    }

    /// 处理单条消息
    async fn handle_message(&mut self, msg: NotificationMessage) {
        match msg {
            NotificationMessage::Notify {
                topic,
                title,
                content,
            } => {
                self.handle_notify(topic, title, content).await;
            }

            NotificationMessage::NotifyWithPhoto {
                title,
                content,
                photo,
                cache_key,
            } => {
                self.handle_notify_with_photo(title, content, photo, cache_key)
                    .await;
            }

            NotificationMessage::InvalidateConfig => {
                self.handle_invalidate_config().await;
            }
        }
    }

    async fn handle_notify(&self, topic: Topic, title: String, content: String) {
        let settings = self.settings.get();
        if !settings.notification.enabled {
            return;
        }

        if let Err(e) = self.worker.notify(topic, title, content).await {
            tracing::error!("Failed to send notification: {}", e);
        }
    }

    async fn handle_notify_with_photo(
        &self,
        title: String,
        content: String,
        photo: Vec<u8>,
        cache_key: Option<String>,
    ) {
        let settings = self.settings.get();
        if !settings.notification.enabled {
            return;
        }

        // Format the caption similar to normal notifications
        let caption = format!(
            "*[下载通知]* {}\n\n{}\n\n_发送时间: {}_",
            title,
            content,
            chrono::Local::now().format("%Y-%m-%d %H:%M:%S")
        );

        if let Err(e) = self
            .worker
            .send_photo_direct(&caption, &photo, "Markdown", cache_key.as_deref())
            .await
        {
            tracing::error!("Failed to send photo notification: {}", e);
        }
    }

    async fn handle_invalidate_config(&mut self) {
        let new_config = self.settings.get().notification.telegram.clone();
        if let Some(notifier) =
            Self::create_telegram_notifier(&new_config, self.http_client.get_client())
        {
            self.worker.set_notifier(notifier);
            tracing::debug!("Notification provider updated due to settings change");
        }
    }
}
