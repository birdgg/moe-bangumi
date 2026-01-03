use std::sync::Arc;

use sqlx::SqlitePool;

use crate::models::{CreateTorrent, Torrent};
use crate::repositories::TorrentRepository;
use crate::services::{AddTaskOptions, DownloaderService};

/// 下载任务参数
pub struct QueueDownloadParams {
    /// 数据库记录信息
    pub torrent: CreateTorrent,
    /// 下载保存路径
    pub save_path: String,
    /// 重命名文件名
    pub rename: String,
}

/// 高层服务：协调 downloader + 数据库操作
#[derive(Clone)]
pub struct TorrentCoordinator {
    db: SqlitePool,
    downloader: Arc<DownloaderService>,
}

impl TorrentCoordinator {
    pub fn new(db: SqlitePool, downloader: Arc<DownloaderService>) -> Self {
        Self { db, downloader }
    }

    /// 创建 torrent 记录并添加下载任务
    ///
    /// 1. 在数据库中创建 torrent 记录
    /// 2. 调用 downloader.add_task() (fire-and-forget)
    pub async fn queue_download(&self, params: QueueDownloadParams) -> Result<Torrent, sqlx::Error> {
        // 1. 创建数据库记录
        let torrent = TorrentRepository::create(&self.db, params.torrent).await?;

        // 2. 添加下载任务 (fire-and-forget)
        let options = AddTaskOptions::new(&torrent.torrent_url)
            .save_path(&params.save_path)
            .rename(&params.rename)
            .add_tag("moe")
            .add_tag("rename");

        self.downloader.add_task(options);
        tracing::debug!("Queued download task: {}", torrent.info_hash);

        Ok(torrent)
    }

    /// 删除 torrent 记录和下载任务
    ///
    /// 1. 从数据库删除 torrent 记录
    /// 2. 调用 downloader.delete_task() (fire-and-forget)
    pub async fn delete_torrent(
        &self,
        torrent_id: i64,
        info_hash: &str,
        delete_files: bool,
    ) -> Result<(), sqlx::Error> {
        // 1. 删除数据库记录
        TorrentRepository::delete(&self.db, torrent_id).await?;

        // 2. 删除下载任务 (fire-and-forget)
        self.downloader.delete_task(&[info_hash], delete_files);
        tracing::debug!("Deleted torrent: {}", info_hash);

        Ok(())
    }
}
