mod client;
mod config;
mod error;
mod impls;
mod models;
mod traits;

pub use client::DownloaderClient;
pub use config::DownloaderConfig;
pub use error::{DownloaderError, Result};
pub use models::{AddTaskOptions, DownloaderType, Task, TaskFile, TaskFilter, TaskStatus};
pub use traits::Downloader;
