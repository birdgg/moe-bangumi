//! Actor-based services
//!
//! Contains all services that use the actor pattern for concurrency.

pub mod downloader;
pub mod log_cleanup;
pub mod metadata;
pub mod notification;
pub mod rename_actor;
pub mod rss_fetch;

// Downloader actor
pub use downloader::{
    create_downloader_service, AddTaskOptions, Downloader, DownloaderClient, DownloaderConfig,
    DownloaderError, DownloaderHandle, DownloaderType, Task, TaskFile, TaskFilter, TaskStatus,
};

// Log cleanup actor
pub use log_cleanup::{create_log_cleanup_actor, LogCleanupHandle};

// Metadata actor
pub use metadata::{
    create_metadata_actor, MetadataError, MetadataHandle, MetadataService, PosterError,
    PosterService,
};

// Notification actor
pub use notification::{
    create_notification_service, NotificationConfig, NotificationError, NotificationHandle,
    NotificationService, Notifier, Topic,
};

// Rename actor
pub use rename_actor::{create_rename_actor, RenameHandle};

// RSS fetch actor
pub use rss_fetch::{create_rss_fetch_actor, RssFetchHandle};
