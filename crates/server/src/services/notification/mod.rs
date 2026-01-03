mod actor;
mod error;

pub use error::NotificationError;

use std::sync::Arc;

use tokio::sync::mpsc;

use actor::{NotificationActor, NotificationHandle, NotificationMessage};

use super::{HttpClientService, SettingsService};

/// NotificationService 类型别名，保持 API 兼容
pub type NotificationService = NotificationHandle;

/// 创建 NotificationService（Actor 模式）
///
/// 返回一个 NotificationHandle，API 与原 NotificationService 兼容。
pub fn create_notification_service(
    settings: Arc<SettingsService>,
    http_client: Arc<HttpClientService>,
) -> NotificationService {
    let (sender, receiver) = mpsc::channel::<NotificationMessage>(32);

    // 启动 Actor
    let actor = NotificationActor::new(Arc::clone(&settings), Arc::clone(&http_client), receiver);
    tokio::spawn(actor.run());

    // 创建 Handle
    let handle = NotificationHandle::new(sender);

    // 启动设置监听任务
    spawn_settings_watcher(settings, handle.clone());

    handle
}

/// 启动设置变化监听任务
fn spawn_settings_watcher(settings: Arc<SettingsService>, handle: NotificationHandle) {
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
                handle.invalidate().await;
            }
            prev_config = new_config;
        }
    });
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
