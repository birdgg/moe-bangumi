use serde::{Deserialize, Serialize};

/// RSS source type with URL
#[derive(Debug, Clone)]
pub enum RssSource {
    /// Mikan RSS feed URL
    Mikan(String),
    /// Nyaa RSS feed URL
    Nyaa(String),
}

impl RssSource {
    /// Get the URL of this RSS source
    pub fn url(&self) -> &str {
        match self {
            RssSource::Mikan(url) | RssSource::Nyaa(url) => url,
        }
    }
}

/// Unified RSS item enum that wraps source-specific items
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "source", rename_all = "lowercase")]
pub enum RssItem {
    Mikan(MikanItem),
    Nyaa(NyaaItem),
}

impl RssItem {
    /// Get the title of the RSS item
    pub fn title(&self) -> &str {
        match self {
            RssItem::Mikan(item) => &item.title,
            RssItem::Nyaa(item) => &item.title,
        }
    }

    /// Get the torrent URL
    pub fn torrent_url(&self) -> &str {
        match self {
            RssItem::Mikan(item) => &item.torrent_url,
            RssItem::Nyaa(item) => &item.torrent_url,
        }
    }

    /// Get the info hash
    pub fn info_hash(&self) -> &str {
        match self {
            RssItem::Mikan(item) => &item.info_hash,
            RssItem::Nyaa(item) => &item.info_hash,
        }
    }
}

/// Mikan-specific RSS item
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MikanItem {
    /// Title of the torrent
    pub title: String,
    /// Direct torrent download URL
    pub torrent_url: String,
    pub info_hash: String,
}

/// Nyaa-specific RSS item with additional metadata
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct NyaaItem {
    /// Title of the torrent
    pub title: String,
    /// Direct torrent download URL
    pub torrent_url: String,
    /// BitTorrent info hash
    pub info_hash: String,
}
