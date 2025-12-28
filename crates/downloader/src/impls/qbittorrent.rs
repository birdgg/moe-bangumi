use async_trait::async_trait;
use qbittorrent::{
    AddTorrentRequest, QBittorrentClient, TorrentFile as QBTorrentFile,
    TorrentFilter as QBTorrentFilter, TorrentInfo as QBTorrentInfo, TorrentInfoRequest,
};

use crate::error::{DownloaderError, Result};
use crate::models::{AddTaskOptions, Task, TaskFile, TaskFilter, TaskStatus};
use crate::traits::Downloader;

/// qBittorrent downloader implementation
pub struct QBittorrentDownloader {
    client: QBittorrentClient,
    username: String,
    password: String,
}

impl QBittorrentDownloader {
    /// Create a new qBittorrent downloader
    pub fn new(
        url: impl Into<String>,
        username: impl Into<String>,
        password: impl Into<String>,
    ) -> Self {
        let client = QBittorrentClient::new(url);
        Self {
            client,
            username: username.into(),
            password: password.into(),
        }
    }
}

// ============================================================================
// Type Conversions: qBittorrent -> Unified Models
// ============================================================================

/// Convert qBittorrent state string to TaskStatus enum
fn parse_status(state: &str) -> TaskStatus {
    match state.to_lowercase().as_str() {
        "downloading" | "metadl" | "forceddl" => TaskStatus::Downloading,
        "uploading" | "stalledup" | "forcedup" => TaskStatus::Seeding,
        "pauseddl" | "pausedup" => TaskStatus::Paused,
        "queueddl" | "queuedup" => TaskStatus::Queued,
        "checkingdl" | "checkingup" | "checkingresumedata" => TaskStatus::Checking,
        "stalleddl" => TaskStatus::Stalled,
        "error" | "missingfiles" => TaskStatus::Error,
        _ => TaskStatus::Unknown,
    }
}

/// Convert qBittorrent TorrentInfo to unified Task
impl From<QBTorrentInfo> for Task {
    fn from(info: QBTorrentInfo) -> Self {
        Self {
            id: info.hash,
            name: info.name,
            status: parse_status(&info.state),
            progress: info.progress,
            save_path: info.save_path,
            total_size: info.size,
            downloaded: info.downloaded,
            eta: info.eta,
            tags: info.tags,
            category: None, // qBittorrent TorrentInfo doesn't include category
        }
    }
}

/// Convert qBittorrent TorrentFile to unified TaskFile
impl From<QBTorrentFile> for TaskFile {
    fn from(file: QBTorrentFile) -> Self {
        Self {
            index: file.index,
            path: file.name,
            size: file.size,
            progress: file.progress,
        }
    }
}

/// Convert unified AddTaskOptions to qBittorrent AddTorrentRequest
impl From<AddTaskOptions> for AddTorrentRequest {
    fn from(options: AddTaskOptions) -> Self {
        let mut request = AddTorrentRequest::with_url(&options.url);

        if let Some(path) = options.save_path {
            request = request.savepath(path);
        }

        if let Some(category) = options.category {
            request = request.category(category);
        }

        if !options.tags.is_empty() {
            request = request.tags(options.tags);
        }

        if let Some(rename) = options.rename {
            request = request.rename(rename);
        }

        request
    }
}

/// Convert TaskStatus to qBittorrent filter
fn status_to_qb_filter(status: TaskStatus) -> QBTorrentFilter {
    match status {
        TaskStatus::Downloading => QBTorrentFilter::Downloading,
        TaskStatus::Seeding => QBTorrentFilter::Seeding,
        TaskStatus::Completed => QBTorrentFilter::Completed,
        TaskStatus::Paused => QBTorrentFilter::Stopped,
        TaskStatus::Stalled => QBTorrentFilter::Stalled,
        TaskStatus::Error => QBTorrentFilter::Errored,
        TaskStatus::Queued | TaskStatus::Checking | TaskStatus::Unknown => QBTorrentFilter::All,
    }
}

/// Convert unified TaskFilter to qBittorrent TorrentInfoRequest
impl From<&TaskFilter> for TorrentInfoRequest {
    fn from(filter: &TaskFilter) -> Self {
        let mut request = TorrentInfoRequest::new();

        if let Some(status) = filter.status {
            request = request.filter(status_to_qb_filter(status));
        }

        if let Some(category) = &filter.category {
            request = request.category(category);
        }

        if let Some(tag) = &filter.tag {
            request = request.tag(tag);
        }

        if let Some(ids) = &filter.ids {
            let hash_refs: Vec<&str> = ids.iter().map(|s| s.as_str()).collect();
            request = request.hashes(&hash_refs);
        }

        request
    }
}

// ============================================================================
// Downloader Trait Implementation
// ============================================================================

#[async_trait]
impl Downloader for QBittorrentDownloader {
    async fn login(&self) -> Result<()> {
        self.client
            .login(&self.username, &self.password)
            .await
            .map_err(DownloaderError::from)?;
        tracing::debug!("qBittorrent authenticated successfully");
        Ok(())
    }

    async fn is_login(&self) -> Result<bool> {
        Ok(self.client.is_authenticated().await)
    }

    async fn add_task(&self, options: AddTaskOptions) -> Result<String> {
        let url = options.url.clone();
        let request: AddTorrentRequest = options.into();

        self.client
            .add_torrent(request)
            .await
            .map_err(DownloaderError::from)?;

        // qBittorrent doesn't return hash, use URL as identifier
        Ok(url)
    }

    async fn delete_task(&self, ids: &[&str], delete_files: bool) -> Result<()> {
        self.client
            .delete_torrents(ids, delete_files)
            .await
            .map_err(DownloaderError::from)?;
        Ok(())
    }

    async fn get_tasks(&self, filter: Option<&TaskFilter>) -> Result<Vec<Task>> {
        let request = filter.map(|f| f.into());

        let torrents = self
            .client
            .get_torrents_info(request)
            .await
            .map_err(DownloaderError::from)?;

        Ok(torrents.into_iter().map(Task::from).collect())
    }

    async fn get_task_files(&self, id: &str) -> Result<Vec<TaskFile>> {
        let files = self
            .client
            .get_torrent_files(id)
            .await
            .map_err(DownloaderError::from)?;

        Ok(files.into_iter().map(TaskFile::from).collect())
    }

    async fn add_tags(&self, id: &str, tags: &[&str]) -> Result<()> {
        self.client
            .add_tags(&[id], tags)
            .await
            .map_err(DownloaderError::from)?;
        Ok(())
    }

    async fn remove_tags(&self, id: &str, tags: &[&str]) -> Result<()> {
        self.client
            .remove_tags(&[id], tags)
            .await
            .map_err(DownloaderError::from)?;
        Ok(())
    }
}
