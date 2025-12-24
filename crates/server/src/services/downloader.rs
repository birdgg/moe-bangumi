mod client;
mod config;
mod error;
mod models;
mod qbittorrent_impl;
mod service;
mod traits;

pub use client::DownloaderClient;
pub use config::DownloaderConfig;
pub use error::DownloaderError;
pub use models::AddTorrentOptions;
pub use service::DownloaderService;
pub use traits::Downloader;

// Re-export DownloaderType from models for convenience
pub use crate::models::DownloaderType;
