use std::sync::Arc;

use tokio::sync::mpsc;

use crate::models::DownloaderSettings;
use crate::services::SettingsService;

mod actor;

use actor::{DownloaderActor, DownloaderHandle, DownloaderMessage};

// Re-export types from downloader crate
pub use downloader::{
    AddTaskOptions, Downloader, DownloaderClient, DownloaderConfig, DownloaderError,
    DownloaderType, Task, TaskFile, TaskFilter, TaskStatus,
};

/// Task tags used internally
mod tags {
    /// Tag identifying tasks added by this application
    pub const MOE: &str = "moe";
    /// Tag identifying tasks pending rename processing
    pub const RENAME: &str = "rename";
}

/// DownloaderService 类型别名，保持 API 兼容
pub type DownloaderService = DownloaderHandle;

/// 创建 DownloaderService（Actor 模式）
///
/// 返回一个 DownloaderHandle，API 与原 DownloaderService 完全兼容。
pub fn create_downloader_service(settings: Arc<SettingsService>) -> DownloaderService {
    let (sender, receiver) = mpsc::channel::<DownloaderMessage>(32);

    // 启动 Actor
    let actor = DownloaderActor::new(Arc::clone(&settings), receiver);
    tokio::spawn(actor.run());

    // 创建 Handle
    let handle = DownloaderHandle::new(sender);

    // 启动设置监听任务
    spawn_settings_watcher(settings, handle.clone());

    handle
}

/// 启动设置变化监听任务
fn spawn_settings_watcher(settings: Arc<SettingsService>, handle: DownloaderHandle) {
    let mut watcher = settings.subscribe();
    let initial_settings = watcher.borrow().downloader.clone();

    tokio::spawn(async move {
        let mut prev_settings = initial_settings;
        loop {
            if watcher.changed().await.is_err() {
                break;
            }
            let new_settings = watcher.borrow().downloader.clone();

            if settings_changed(&prev_settings, &new_settings) {
                handle.invalidate().await;
            }
            prev_settings = new_settings;
        }
    });
}

/// 检查设置是否变化
fn settings_changed(old: &DownloaderSettings, new: &DownloaderSettings) -> bool {
    old != new
}
