use chrono::{DateTime, Utc};
use sqlx::SqlitePool;

use crate::models::{CreateDownloadTask, DownloadTask, DownloadTaskStatus, UpdateDownloadTask};

/// Common SELECT fields for download task queries
const SELECT_DOWNLOAD_TASK: &str = r#"
    SELECT
        id, created_at, updated_at,
        torrent_id, status, error_message
    FROM download_task
"#;

pub struct DownloadTaskRepository;

impl DownloadTaskRepository {
    /// Create a new download task
    pub async fn create(
        pool: &SqlitePool,
        data: CreateDownloadTask,
    ) -> Result<DownloadTask, sqlx::Error> {
        let result = sqlx::query(
            r#"
            INSERT INTO download_task (torrent_id, status)
            VALUES ($1, $2)
            RETURNING id
            "#,
        )
        .bind(data.torrent_id)
        .bind(DownloadTaskStatus::default().as_str())
        .fetch_one(pool)
        .await?;

        let id: i64 = sqlx::Row::get(&result, "id");
        Self::get_by_id(pool, id)
            .await?
            .ok_or(sqlx::Error::RowNotFound)
    }

    /// Get a download task by ID
    pub async fn get_by_id(
        pool: &SqlitePool,
        id: i64,
    ) -> Result<Option<DownloadTask>, sqlx::Error> {
        let query = format!("{} WHERE id = $1", SELECT_DOWNLOAD_TASK);
        let row = sqlx::query_as::<_, DownloadTaskRow>(&query)
            .bind(id)
            .fetch_optional(pool)
            .await?;

        Ok(row.map(Into::into))
    }

    /// Get all download tasks for a torrent
    pub async fn get_by_torrent_id(
        pool: &SqlitePool,
        torrent_id: i64,
    ) -> Result<Vec<DownloadTask>, sqlx::Error> {
        let query = format!(
            "{} WHERE torrent_id = $1 ORDER BY created_at DESC",
            SELECT_DOWNLOAD_TASK
        );
        let rows = sqlx::query_as::<_, DownloadTaskRow>(&query)
            .bind(torrent_id)
            .fetch_all(pool)
            .await?;

        Ok(rows.into_iter().map(Into::into).collect())
    }

    /// Get the latest download task for a torrent
    pub async fn get_latest_by_torrent_id(
        pool: &SqlitePool,
        torrent_id: i64,
    ) -> Result<Option<DownloadTask>, sqlx::Error> {
        let query = format!(
            "{} WHERE torrent_id = $1 ORDER BY created_at DESC LIMIT 1",
            SELECT_DOWNLOAD_TASK
        );
        let row = sqlx::query_as::<_, DownloadTaskRow>(&query)
            .bind(torrent_id)
            .fetch_optional(pool)
            .await?;

        Ok(row.map(Into::into))
    }

    /// Get all download tasks with a specific status
    pub async fn get_by_status(
        pool: &SqlitePool,
        status: DownloadTaskStatus,
    ) -> Result<Vec<DownloadTask>, sqlx::Error> {
        let query = format!(
            "{} WHERE status = $1 ORDER BY created_at ASC",
            SELECT_DOWNLOAD_TASK
        );
        let rows = sqlx::query_as::<_, DownloadTaskRow>(&query)
            .bind(status.as_str())
            .fetch_all(pool)
            .await?;

        Ok(rows.into_iter().map(Into::into).collect())
    }

    /// Get all pending download tasks
    pub async fn get_pending(pool: &SqlitePool) -> Result<Vec<DownloadTask>, sqlx::Error> {
        Self::get_by_status(pool, DownloadTaskStatus::Pending).await
    }

    /// Get all downloading tasks
    pub async fn get_downloading(pool: &SqlitePool) -> Result<Vec<DownloadTask>, sqlx::Error> {
        Self::get_by_status(pool, DownloadTaskStatus::Downloading).await
    }

    /// Get all active tasks (pending, paused, or downloading)
    pub async fn get_active(pool: &SqlitePool) -> Result<Vec<DownloadTask>, sqlx::Error> {
        let query = format!(
            "{} WHERE status IN ('pending', 'paused', 'downloading') ORDER BY created_at ASC",
            SELECT_DOWNLOAD_TASK
        );
        let rows = sqlx::query_as::<_, DownloadTaskRow>(&query)
            .fetch_all(pool)
            .await?;

        Ok(rows.into_iter().map(Into::into).collect())
    }

    /// Update a download task
    pub async fn update(
        pool: &SqlitePool,
        id: i64,
        data: UpdateDownloadTask,
    ) -> Result<Option<DownloadTask>, sqlx::Error> {
        let existing = Self::get_by_id(pool, id).await?;
        let Some(existing) = existing else {
            return Ok(None);
        };

        let status = data.status.unwrap_or(existing.status);
        let error_message = data.error_message.resolve(existing.error_message);

        sqlx::query(
            r#"
            UPDATE download_task SET
                status = $1,
                error_message = $2
            WHERE id = $3
            "#,
        )
        .bind(status.as_str())
        .bind(&error_message)
        .bind(id)
        .execute(pool)
        .await?;

        Self::get_by_id(pool, id).await
    }

    /// Update task status
    pub async fn update_status(
        pool: &SqlitePool,
        id: i64,
        status: DownloadTaskStatus,
    ) -> Result<bool, sqlx::Error> {
        let result = sqlx::query("UPDATE download_task SET status = $1 WHERE id = $2")
            .bind(status.as_str())
            .bind(id)
            .execute(pool)
            .await?;

        Ok(result.rows_affected() > 0)
    }

    /// Mark task as failed with error message
    pub async fn mark_failed(
        pool: &SqlitePool,
        id: i64,
        error_message: &str,
    ) -> Result<bool, sqlx::Error> {
        let result = sqlx::query(
            "UPDATE download_task SET status = 'failed', error_message = $1 WHERE id = $2",
        )
        .bind(error_message)
        .bind(id)
        .execute(pool)
        .await?;

        Ok(result.rows_affected() > 0)
    }

    /// Mark task as completed
    pub async fn mark_completed(pool: &SqlitePool, id: i64) -> Result<bool, sqlx::Error> {
        Self::update_status(pool, id, DownloadTaskStatus::Completed).await
    }

    /// Delete a download task by ID
    pub async fn delete(pool: &SqlitePool, id: i64) -> Result<bool, sqlx::Error> {
        let result = sqlx::query("DELETE FROM download_task WHERE id = $1")
            .bind(id)
            .execute(pool)
            .await?;

        Ok(result.rows_affected() > 0)
    }

    /// Delete all download tasks for a torrent
    pub async fn delete_by_torrent_id(
        pool: &SqlitePool,
        torrent_id: i64,
    ) -> Result<u64, sqlx::Error> {
        let result = sqlx::query("DELETE FROM download_task WHERE torrent_id = $1")
            .bind(torrent_id)
            .execute(pool)
            .await?;

        Ok(result.rows_affected())
    }

    /// Delete old completed tasks (older than given number of days)
    pub async fn delete_old_completed(pool: &SqlitePool, days: i32) -> Result<u64, sqlx::Error> {
        let result = sqlx::query(
            r#"
            DELETE FROM download_task
            WHERE status = 'completed'
            AND created_at < datetime('now', $1)
            "#,
        )
        .bind(format!("-{} days", days))
        .execute(pool)
        .await?;

        Ok(result.rows_affected())
    }
}

/// Internal row type for mapping SQLite results
#[derive(Debug, sqlx::FromRow)]
struct DownloadTaskRow {
    id: i64,
    created_at: DateTime<Utc>,
    updated_at: DateTime<Utc>,
    torrent_id: i64,
    status: String,
    error_message: Option<String>,
}

impl From<DownloadTaskRow> for DownloadTask {
    fn from(row: DownloadTaskRow) -> Self {
        Self {
            id: row.id,
            created_at: row.created_at,
            updated_at: row.updated_at,
            torrent_id: row.torrent_id,
            status: row.status.parse().unwrap_or_default(),
            error_message: row.error_message,
        }
    }
}
