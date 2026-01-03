use std::sync::Arc;

use downloader::{Downloader, DownloaderError};
use tokio::sync::mpsc;

use super::messages::DownloaderMessage;
use super::state::DownloaderActorState;
use crate::services::SettingsService;

/// Actor 主循环
pub struct DownloaderActor {
    state: DownloaderActorState,
    receiver: mpsc::Receiver<DownloaderMessage>,
}

impl DownloaderActor {
    pub fn new(settings: Arc<SettingsService>, receiver: mpsc::Receiver<DownloaderMessage>) -> Self {
        Self {
            state: DownloaderActorState::new(settings),
            receiver,
        }
    }

    /// 运行 Actor 主循环
    pub async fn run(mut self) {
        tracing::info!("Downloader actor started");

        while let Some(msg) = self.receiver.recv().await {
            self.handle_message(msg).await;
        }

        tracing::info!("Downloader actor stopped");
    }

    /// 处理单条消息
    async fn handle_message(&mut self, msg: DownloaderMessage) {
        match msg {
            DownloaderMessage::InvalidateClient => {
                self.state.invalidate();
            }

            DownloaderMessage::IsAvailable { reply } => {
                let result = self.state.ensure_client().await.is_ok();
                let _ = reply.send(result);
            }

            DownloaderMessage::AddTask { options } => {
                if let Err(e) = self.with_retry_add_task(options).await {
                    tracing::error!("Failed to add task: {}", e);
                }
            }

            DownloaderMessage::GetTasks { filter, reply } => {
                let result = self.with_retry_get_tasks(filter).await;
                let _ = reply.send(result);
            }

            DownloaderMessage::GetTaskFiles { hash, reply } => {
                let result = self.with_retry_get_task_files(&hash).await;
                let _ = reply.send(result);
            }

            DownloaderMessage::DeleteTask { ids, delete_files } => {
                if let Err(e) = self.with_retry_delete_task(&ids, delete_files).await {
                    tracing::error!("Failed to delete task: {}", e);
                }
            }

            DownloaderMessage::AddTags { id, tags, reply } => {
                let result = self.with_retry_add_tags(&id, &tags).await;
                let _ = reply.send(result);
            }

            DownloaderMessage::RemoveTags { id, tags, reply } => {
                let result = self.with_retry_remove_tags(&id, &tags).await;
                let _ = reply.send(result);
            }

            DownloaderMessage::RenameFile {
                id,
                old_path,
                new_path,
                reply,
            } => {
                let result = self.with_retry_rename_file(&id, &old_path, &new_path).await;
                let _ = reply.send(result);
            }
        }
    }

    /// 添加任务（带重试）
    async fn with_retry_add_task(
        &mut self,
        options: downloader::AddTaskOptions,
    ) -> Result<String, DownloaderError> {
        self.state.ensure_client().await?;

        let opts = options.add_tag("rename");
        let result = self.state.client().unwrap().add_task(opts.clone()).await;

        match result {
            Ok(v) => Ok(v),
            Err(e) if DownloaderActorState::is_auth_error(&e) => {
                tracing::warn!("Auth error detected, attempting re-authentication: {}", e);
                self.state.reauthenticate().await?;
                self.state.client().unwrap().add_task(opts).await
            }
            Err(e) => Err(e),
        }
    }

    /// 获取任务列表（带重试）
    async fn with_retry_get_tasks(
        &mut self,
        filter: Option<downloader::TaskFilter>,
    ) -> Result<Vec<downloader::Task>, DownloaderError> {
        self.state.ensure_client().await?;

        let result = self
            .state
            .client()
            .unwrap()
            .get_tasks(filter.as_ref())
            .await;

        match result {
            Ok(v) => Ok(v),
            Err(e) if DownloaderActorState::is_auth_error(&e) => {
                tracing::warn!("Auth error detected, attempting re-authentication: {}", e);
                self.state.reauthenticate().await?;
                self.state
                    .client()
                    .unwrap()
                    .get_tasks(filter.as_ref())
                    .await
            }
            Err(e) => Err(e),
        }
    }

    /// 获取任务文件（带重试）
    async fn with_retry_get_task_files(
        &mut self,
        hash: &str,
    ) -> Result<Vec<downloader::TaskFile>, DownloaderError> {
        self.state.ensure_client().await?;

        let result = self.state.client().unwrap().get_task_files(hash).await;

        match result {
            Ok(v) => Ok(v),
            Err(e) if DownloaderActorState::is_auth_error(&e) => {
                tracing::warn!("Auth error detected, attempting re-authentication: {}", e);
                self.state.reauthenticate().await?;
                self.state.client().unwrap().get_task_files(hash).await
            }
            Err(e) => Err(e),
        }
    }

    /// 删除任务（带重试）
    async fn with_retry_delete_task(
        &mut self,
        ids: &[String],
        delete_files: bool,
    ) -> Result<(), DownloaderError> {
        self.state.ensure_client().await?;

        let id_refs: Vec<&str> = ids.iter().map(|s| s.as_str()).collect();
        let result = self
            .state
            .client()
            .unwrap()
            .delete_task(&id_refs, delete_files)
            .await;

        match result {
            Ok(v) => Ok(v),
            Err(e) if DownloaderActorState::is_auth_error(&e) => {
                tracing::warn!("Auth error detected, attempting re-authentication: {}", e);
                self.state.reauthenticate().await?;
                self.state
                    .client()
                    .unwrap()
                    .delete_task(&id_refs, delete_files)
                    .await
            }
            Err(e) => Err(e),
        }
    }

    /// 添加标签（带重试）
    async fn with_retry_add_tags(
        &mut self,
        id: &str,
        tags: &[String],
    ) -> Result<(), DownloaderError> {
        self.state.ensure_client().await?;

        let tag_refs: Vec<&str> = tags.iter().map(|s| s.as_str()).collect();
        let result = self.state.client().unwrap().add_tags(id, &tag_refs).await;

        match result {
            Ok(v) => Ok(v),
            Err(e) if DownloaderActorState::is_auth_error(&e) => {
                tracing::warn!("Auth error detected, attempting re-authentication: {}", e);
                self.state.reauthenticate().await?;
                self.state.client().unwrap().add_tags(id, &tag_refs).await
            }
            Err(e) => Err(e),
        }
    }

    /// 移除标签（带重试）
    async fn with_retry_remove_tags(
        &mut self,
        id: &str,
        tags: &[String],
    ) -> Result<(), DownloaderError> {
        self.state.ensure_client().await?;

        let tag_refs: Vec<&str> = tags.iter().map(|s| s.as_str()).collect();
        let result = self
            .state
            .client()
            .unwrap()
            .remove_tags(id, &tag_refs)
            .await;

        match result {
            Ok(v) => Ok(v),
            Err(e) if DownloaderActorState::is_auth_error(&e) => {
                tracing::warn!("Auth error detected, attempting re-authentication: {}", e);
                self.state.reauthenticate().await?;
                self.state
                    .client()
                    .unwrap()
                    .remove_tags(id, &tag_refs)
                    .await
            }
            Err(e) => Err(e),
        }
    }

    /// 重命名文件（带重试）
    async fn with_retry_rename_file(
        &mut self,
        id: &str,
        old_path: &str,
        new_path: &str,
    ) -> Result<(), DownloaderError> {
        self.state.ensure_client().await?;

        let result = self
            .state
            .client()
            .unwrap()
            .rename_file(id, old_path, new_path)
            .await;

        match result {
            Ok(v) => Ok(v),
            Err(e) if DownloaderActorState::is_auth_error(&e) => {
                tracing::warn!("Auth error detected, attempting re-authentication: {}", e);
                self.state.reauthenticate().await?;
                self.state
                    .client()
                    .unwrap()
                    .rename_file(id, old_path, new_path)
                    .await
            }
            Err(e) => Err(e),
        }
    }
}
