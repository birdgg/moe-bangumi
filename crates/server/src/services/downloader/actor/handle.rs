use downloader::{AddTaskOptions, DownloaderError, Task, TaskFile, TaskFilter};
use tokio::sync::{mpsc, oneshot};

use super::messages::DownloaderMessage;

/// DownloaderService 的对外接口（Handle）
///
/// 这个结构体提供与原 DownloaderService 完全兼容的 API，
/// 内部通过 channel 与 Actor 通信。
#[derive(Clone)]
pub struct DownloaderHandle {
    sender: mpsc::Sender<DownloaderMessage>,
}

impl DownloaderHandle {
    pub fn new(sender: mpsc::Sender<DownloaderMessage>) -> Self {
        Self { sender }
    }

    /// 检查 downloader 是否可用
    pub async fn is_available(&self) -> bool {
        let (reply, rx) = oneshot::channel();
        if self
            .sender
            .send(DownloaderMessage::IsAvailable { reply })
            .await
            .is_err()
        {
            return false;
        }
        rx.await.unwrap_or(false)
    }

    /// 添加下载任务 (fire-and-forget)
    pub fn add_task(&self, options: AddTaskOptions) {
        let sender = self.sender.clone();
        tokio::spawn(async move {
            let _ = sender.send(DownloaderMessage::AddTask { options }).await;
        });
    }

    /// 获取任务文件
    pub async fn get_task_files(&self, hash: &str) -> Result<Vec<TaskFile>, DownloaderError> {
        let (reply, rx) = oneshot::channel();
        self.sender
            .send(DownloaderMessage::GetTaskFiles {
                hash: hash.to_string(),
                reply,
            })
            .await
            .map_err(|_| DownloaderError::NotConfigured)?;
        rx.await.map_err(|_| DownloaderError::NotConfigured)?
    }

    /// 获取任务列表
    pub async fn get_tasks(
        &self,
        filter: Option<&TaskFilter>,
    ) -> Result<Vec<Task>, DownloaderError> {
        let (reply, rx) = oneshot::channel();
        self.sender
            .send(DownloaderMessage::GetTasks {
                filter: filter.cloned(),
                reply,
            })
            .await
            .map_err(|_| DownloaderError::NotConfigured)?;
        rx.await.map_err(|_| DownloaderError::NotConfigured)?
    }

    /// 删除任务 (fire-and-forget)
    pub fn delete_task(&self, ids: &[&str], delete_files: bool) {
        let sender = self.sender.clone();
        let ids: Vec<String> = ids.iter().map(|s| s.to_string()).collect();
        tokio::spawn(async move {
            let _ = sender
                .send(DownloaderMessage::DeleteTask { ids, delete_files })
                .await;
        });
    }

    /// 添加标签
    pub async fn add_tags(&self, id: &str, tags: &[&str]) -> Result<(), DownloaderError> {
        let (reply, rx) = oneshot::channel();
        self.sender
            .send(DownloaderMessage::AddTags {
                id: id.to_string(),
                tags: tags.iter().map(|s| s.to_string()).collect(),
                reply,
            })
            .await
            .map_err(|_| DownloaderError::NotConfigured)?;
        rx.await.map_err(|_| DownloaderError::NotConfigured)?
    }

    /// 移除标签
    pub async fn remove_tags(&self, id: &str, tags: &[&str]) -> Result<(), DownloaderError> {
        let (reply, rx) = oneshot::channel();
        self.sender
            .send(DownloaderMessage::RemoveTags {
                id: id.to_string(),
                tags: tags.iter().map(|s| s.to_string()).collect(),
                reply,
            })
            .await
            .map_err(|_| DownloaderError::NotConfigured)?;
        rx.await.map_err(|_| DownloaderError::NotConfigured)?
    }

    /// 重命名文件
    pub async fn rename_file(
        &self,
        id: &str,
        old_path: &str,
        new_path: &str,
    ) -> Result<(), DownloaderError> {
        let (reply, rx) = oneshot::channel();
        self.sender
            .send(DownloaderMessage::RenameFile {
                id: id.to_string(),
                old_path: old_path.to_string(),
                new_path: new_path.to_string(),
                reply,
            })
            .await
            .map_err(|_| DownloaderError::NotConfigured)?;
        rx.await.map_err(|_| DownloaderError::NotConfigured)?
    }

    /// 发送失效通知（内部使用）
    pub(crate) async fn invalidate(&self) {
        let _ = self.sender.send(DownloaderMessage::InvalidateClient).await;
    }
}
