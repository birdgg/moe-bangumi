use super::DownloaderType;
use downloader::DownloaderConfig;
use serde::{Deserialize, Serialize};
use utoipa::ToSchema;

use super::Clearable;

/// Application settings stored in TOML file
#[derive(Debug, Clone, Default, Serialize, Deserialize, ToSchema)]
pub struct Settings {
    /// Downloader configuration
    #[serde(default)]
    pub downloader: DownloaderSettings,
    /// Filter configuration
    #[serde(default)]
    pub filter: FilterSettings,
    /// Proxy configuration for HTTP client
    #[serde(default)]
    pub proxy: ProxySettings,
}

/// Downloader configuration with per-type configs
#[derive(Debug, Clone, Serialize, Deserialize, ToSchema)]
pub struct DownloaderSettings {
    /// Currently active downloader type
    #[serde(default = "DownloaderSettings::default_type", rename = "type")]
    pub downloader_type: DownloaderType,
    /// Default save path for downloads (shared across all downloaders)
    #[serde(default = "DownloaderSettings::default_save_path")]
    pub save_path: String,
    /// Per-downloader configurations
    #[serde(default)]
    pub configs: DownloaderConfigs,
}

/// Per-downloader configurations
#[derive(Debug, Clone, Default, Serialize, Deserialize, ToSchema)]
pub struct DownloaderConfigs {
    /// qBittorrent configuration
    #[serde(default)]
    pub qbittorrent: QBittorrentConfig,
    /// Transmission configuration
    #[serde(default)]
    pub transmission: TransmissionConfig,
}

/// qBittorrent-specific configuration
#[derive(Debug, Clone, Serialize, Deserialize, ToSchema)]
pub struct QBittorrentConfig {
    /// Web UI URL (e.g., http://localhost:8080)
    #[serde(default = "QBittorrentConfig::default_url")]
    pub url: String,
    /// Username (required)
    #[serde(default)]
    pub username: String,
    /// Password (required)
    #[serde(default)]
    pub password: String,
}

impl Default for QBittorrentConfig {
    fn default() -> Self {
        Self {
            url: Self::default_url(),
            username: String::new(),
            password: String::new(),
        }
    }
}

impl QBittorrentConfig {
    fn default_url() -> String {
        "http://localhost:8080".to_string()
    }
}

/// Transmission-specific configuration
#[derive(Debug, Clone, Serialize, Deserialize, ToSchema)]
pub struct TransmissionConfig {
    /// RPC URL (e.g., http://localhost:9091/transmission/rpc)
    #[serde(default = "TransmissionConfig::default_url")]
    pub url: String,
    /// Username (optional)
    #[serde(default)]
    pub username: String,
    /// Password (optional)
    #[serde(default)]
    pub password: String,
}

impl Default for TransmissionConfig {
    fn default() -> Self {
        Self {
            url: Self::default_url(),
            username: String::new(),
            password: String::new(),
        }
    }
}

impl TransmissionConfig {
    fn default_url() -> String {
        "http://localhost:9091/transmission/rpc".to_string()
    }
}

impl Default for DownloaderSettings {
    fn default() -> Self {
        Self {
            downloader_type: Self::default_type(),
            save_path: Self::default_save_path(),
            configs: DownloaderConfigs::default(),
        }
    }
}

impl DownloaderSettings {
    fn default_type() -> DownloaderType {
        DownloaderType::QBittorrent
    }

    fn default_save_path() -> String {
        "/Media/Bangumi".to_string()
    }

    /// Get the active downloader's configuration
    pub fn get_active_config(&self) -> DownloaderConfig {
        match self.downloader_type {
            DownloaderType::QBittorrent => {
                let cfg = &self.configs.qbittorrent;
                DownloaderConfig {
                    downloader_type: DownloaderType::QBittorrent,
                    url: cfg.url.clone(),
                    username: Some(cfg.username.clone()),
                    password: Some(cfg.password.clone()),
                }
            }
            DownloaderType::Transmission => {
                let cfg = &self.configs.transmission;
                DownloaderConfig {
                    downloader_type: DownloaderType::Transmission,
                    url: cfg.url.clone(),
                    username: if cfg.username.is_empty() {
                        None
                    } else {
                        Some(cfg.username.clone())
                    },
                    password: if cfg.password.is_empty() {
                        None
                    } else {
                        Some(cfg.password.clone())
                    },
                }
            }
        }
    }

    /// Check if the active downloader config is complete
    pub fn is_active_config_complete(&self) -> bool {
        match self.downloader_type {
            DownloaderType::QBittorrent => {
                let cfg = &self.configs.qbittorrent;
                !cfg.url.is_empty() && !cfg.username.is_empty() && !cfg.password.is_empty()
            }
            DownloaderType::Transmission => {
                // Transmission only requires URL
                !self.configs.transmission.url.is_empty()
            }
        }
    }
}

/// Filter configuration
#[derive(Debug, Clone, Serialize, Deserialize, ToSchema)]
pub struct FilterSettings {
    /// Global RSS filters (regex patterns to exclude)
    #[serde(default = "FilterSettings::default_global_rss_filters")]
    pub global_rss_filters: Vec<String>,
}

impl Default for FilterSettings {
    fn default() -> Self {
        Self {
            global_rss_filters: Self::default_global_rss_filters(),
        }
    }
}

impl FilterSettings {
    fn default_global_rss_filters() -> Vec<String> {
        vec![
            "720[Pp]".to_string(),
            "\\d-\\d".to_string(),
            "合集".to_string(),
        ]
    }
}

/// Proxy configuration for HTTP client
#[derive(Debug, Clone, Serialize, Deserialize, ToSchema)]
pub struct ProxySettings {
    /// Proxy server URL (e.g., http://127.0.0.1:7890 or socks5://127.0.0.1:1080)
    #[serde(default)]
    pub url: String,
    /// Proxy username (optional)
    #[serde(default)]
    pub username: String,
    /// Proxy password (optional)
    #[serde(default)]
    pub password: String,
}

impl Default for ProxySettings {
    fn default() -> Self {
        Self {
            url: String::new(),
            username: String::new(),
            password: String::new(),
        }
    }
}

impl Settings {
    /// Merge update data into current settings
    pub fn merge(&self, update: UpdateSettings) -> Self {
        Self {
            downloader: if let Some(d) = update.downloader {
                DownloaderSettings {
                    downloader_type: d
                        .downloader_type
                        .unwrap_or(self.downloader.downloader_type),
                    save_path: d
                        .save_path
                        .resolve_or_empty(self.downloader.save_path.clone()),
                    configs: DownloaderConfigs {
                        qbittorrent: if let Some(qb) = d.qbittorrent {
                            QBittorrentConfig {
                                url: qb
                                    .url
                                    .resolve_or_empty(self.downloader.configs.qbittorrent.url.clone()),
                                username: qb.username.resolve_or_empty(
                                    self.downloader.configs.qbittorrent.username.clone(),
                                ),
                                password: qb.password.resolve_or_empty(
                                    self.downloader.configs.qbittorrent.password.clone(),
                                ),
                            }
                        } else {
                            self.downloader.configs.qbittorrent.clone()
                        },
                        transmission: if let Some(tr) = d.transmission {
                            TransmissionConfig {
                                url: tr.url.resolve_or_empty(
                                    self.downloader.configs.transmission.url.clone(),
                                ),
                                username: tr.username.resolve_or_empty(
                                    self.downloader.configs.transmission.username.clone(),
                                ),
                                password: tr.password.resolve_or_empty(
                                    self.downloader.configs.transmission.password.clone(),
                                ),
                            }
                        } else {
                            self.downloader.configs.transmission.clone()
                        },
                    },
                }
            } else {
                self.downloader.clone()
            },
            filter: FilterSettings {
                global_rss_filters: update
                    .filter
                    .and_then(|f| f.global_rss_filters)
                    .unwrap_or_else(|| self.filter.global_rss_filters.clone()),
            },
            proxy: ProxySettings {
                url: update
                    .proxy
                    .as_ref()
                    .map(|p| p.url.clone())
                    .unwrap_or(Clearable::Unchanged)
                    .resolve_or_empty(self.proxy.url.clone()),
                username: update
                    .proxy
                    .as_ref()
                    .map(|p| p.username.clone())
                    .unwrap_or(Clearable::Unchanged)
                    .resolve_or_empty(self.proxy.username.clone()),
                password: update
                    .proxy
                    .as_ref()
                    .map(|p| p.password.clone())
                    .unwrap_or(Clearable::Unchanged)
                    .resolve_or_empty(self.proxy.password.clone()),
            },
        }
    }
}

/// Request body for updating settings.
/// All fields are optional - only provided fields will be updated.
#[derive(Debug, Clone, Default, Deserialize, ToSchema)]
pub struct UpdateSettings {
    /// Downloader configuration updates
    #[serde(default)]
    pub downloader: Option<UpdateDownloaderSettings>,
    /// Filter configuration updates
    #[serde(default)]
    pub filter: Option<UpdateFilterSettings>,
    /// Proxy configuration updates
    #[serde(default)]
    pub proxy: Option<UpdateProxySettings>,
}

/// Request body for updating downloader settings
#[derive(Debug, Clone, Default, Deserialize, ToSchema)]
pub struct UpdateDownloaderSettings {
    /// Switch active downloader type
    #[serde(default, rename = "type")]
    pub downloader_type: Option<DownloaderType>,
    /// Update shared save path (send null to clear)
    #[serde(default)]
    #[schema(value_type = Option<String>)]
    pub save_path: Clearable<String>,
    /// Update qBittorrent config
    #[serde(default)]
    pub qbittorrent: Option<UpdateQBittorrentConfig>,
    /// Update Transmission config
    #[serde(default)]
    pub transmission: Option<UpdateTransmissionConfig>,
}

/// Request body for updating qBittorrent settings
#[derive(Debug, Clone, Default, Deserialize, ToSchema)]
pub struct UpdateQBittorrentConfig {
    /// Web UI URL (send null to clear)
    #[serde(default)]
    #[schema(value_type = Option<String>)]
    pub url: Clearable<String>,
    /// Username (send null to clear)
    #[serde(default)]
    #[schema(value_type = Option<String>)]
    pub username: Clearable<String>,
    /// Password (send null to clear)
    #[serde(default)]
    #[schema(value_type = Option<String>)]
    pub password: Clearable<String>,
}

/// Request body for updating Transmission settings
#[derive(Debug, Clone, Default, Deserialize, ToSchema)]
pub struct UpdateTransmissionConfig {
    /// RPC URL (send null to clear)
    #[serde(default)]
    #[schema(value_type = Option<String>)]
    pub url: Clearable<String>,
    /// Username (send null to clear)
    #[serde(default)]
    #[schema(value_type = Option<String>)]
    pub username: Clearable<String>,
    /// Password (send null to clear)
    #[serde(default)]
    #[schema(value_type = Option<String>)]
    pub password: Clearable<String>,
}

/// Request body for updating filter settings
#[derive(Debug, Clone, Default, Deserialize, ToSchema)]
pub struct UpdateFilterSettings {
    /// Global RSS filters (replaces entire array if provided)
    #[serde(default)]
    pub global_rss_filters: Option<Vec<String>>,
}

/// Request body for updating proxy settings
#[derive(Debug, Clone, Default, Deserialize, ToSchema)]
pub struct UpdateProxySettings {
    /// Proxy server URL (send null to clear)
    #[serde(default)]
    #[schema(value_type = Option<String>)]
    pub url: Clearable<String>,
    /// Username (send null to clear)
    #[serde(default)]
    #[schema(value_type = Option<String>)]
    pub username: Clearable<String>,
    /// Password (send null to clear)
    #[serde(default)]
    #[schema(value_type = Option<String>)]
    pub password: Clearable<String>,
}
