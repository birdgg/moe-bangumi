use serde::{Deserialize, Serialize};

/// Options for adding a download task.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct AddTaskOptions {
    /// Download URL (HTTP, magnet, torrent file URL)
    pub url: String,

    /// Save path / download directory
    pub save_path: Option<String>,

    /// Category (if supported by downloader)
    pub category: Option<String>,

    /// Tags/labels
    pub tags: Vec<String>,

    /// Rename content (final name)
    pub rename: Option<String>,
}

impl AddTaskOptions {
    /// Create new options with a URL
    pub fn new(url: impl Into<String>) -> Self {
        Self {
            url: url.into(),
            ..Default::default()
        }
    }

    /// Set save path (builder pattern)
    pub fn save_path(mut self, path: impl Into<String>) -> Self {
        self.save_path = Some(path.into());
        self
    }

    /// Set category (builder pattern)
    pub fn category(mut self, category: impl Into<String>) -> Self {
        self.category = Some(category.into());
        self
    }

    /// Set tags (builder pattern)
    pub fn tags(mut self, tags: impl IntoIterator<Item = impl Into<String>>) -> Self {
        self.tags = tags.into_iter().map(Into::into).collect();
        self
    }

    /// Add a single tag (builder pattern)
    pub fn add_tag(mut self, tag: impl Into<String>) -> Self {
        self.tags.push(tag.into());
        self
    }

    /// Set rename (builder pattern)
    pub fn rename(mut self, name: impl Into<String>) -> Self {
        self.rename = Some(name.into());
        self
    }
}
