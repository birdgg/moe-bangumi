use std::sync::Arc;

use notify::{NotificationConfig, NotificationHandle, TelegramConfig};
use thiserror::Error;

use super::{HttpClientService, SettingsService};

/// NotificationService 类型别名，保持 API 兼容
pub type NotificationService = NotificationHandle;

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

/// 创建 NotificationService
pub fn create_notification_service(
    settings: Arc<SettingsService>,
    http_client: Arc<HttpClientService>,
) -> NotificationService {
    // 从 settings 创建初始配置
    let initial_config = to_notify_config(&settings.get().notification);

    // 创建通知服务
    let handle = notify::create_notification_service(initial_config, http_client.get_client());

    // 启动配置监听任务
    spawn_notification_config_watcher(settings, http_client, handle.clone());

    handle
}

/// 启动配置变化监听任务
fn spawn_notification_config_watcher(
    settings: Arc<SettingsService>,
    http_client: Arc<HttpClientService>,
    handle: NotificationHandle,
) {
    let mut watcher = settings.subscribe();
    let initial_config = watcher.borrow().notification.clone();

    tokio::spawn(async move {
        let mut prev_config = initial_config;
        loop {
            if watcher.changed().await.is_err() {
                break;
            }
            let new_config = watcher.borrow().notification.clone();

            if settings_changed(&prev_config, &new_config) {
                let config = to_notify_config(&new_config);
                handle
                    .update_config(config, http_client.get_client())
                    .await;
            }
            prev_config = new_config;
        }
    });
}

/// 将 server 的配置转换为 notify 的配置
fn to_notify_config(s: &crate::models::NotificationSettings) -> NotificationConfig {
    NotificationConfig {
        enabled: s.enabled,
        telegram: TelegramConfig {
            enabled: s.telegram.enabled,
            bot_token: s.telegram.bot_token.clone(),
            chat_id: s.telegram.chat_id.clone(),
        },
    }
}

/// 检查设置是否变化
fn settings_changed(
    old: &crate::models::NotificationSettings,
    new: &crate::models::NotificationSettings,
) -> bool {
    // Check if enabled state changed
    if old.enabled != new.enabled {
        return true;
    }

    // Check telegram config
    let old_tg = &old.telegram;
    let new_tg = &new.telegram;

    old_tg.enabled != new_tg.enabled
        || old_tg.bot_token != new_tg.bot_token
        || old_tg.chat_id != new_tg.chat_id
}
