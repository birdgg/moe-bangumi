# Notification System Design

## Overview
This document outlines the design for a notification system in `moe-bangumi`. The system's primary goal is to alert users via Telegram when a new episode download finishes.

## Architecture

The notification system follows a modular provider-based architecture, integrated into the existing `SchedulerService`.

```
┌─────────────────────────────────────────────────────────────┐
│                       NotificationJob                       │
│  (Polls DownloaderService for completed tasks)              │
└─────────────────────────┬───────────────────────────────────┘
                          │
                          ▼
┌─────────────────────────────────────────────────────────────┐
│                     NotificationService                     │
│  (Manages providers, formats messages, orchestrates send)   │
└─────────────────────────┬───────────────────────────────────┘
                          │
                          ▼
┌─────────────────────────────────────────────────────────────┐
│                   NotificationProvider Trait                │
│  (Common interface for sending text/images)                 │
└─────────────────────────┬───────────────────────────────────┘
                          │
                          ▼
                  ┌──────────────┐
                  │   Telegram   │
                  └──────────────┘
```

## Core Components

### 1. NotificationProvider Trait

A common interface ensuring all providers support basic text and image notifications.

```rust
// crates/server/src/services/notification/traits.rs

#[async_trait]
pub trait NotificationProvider: Send + Sync {
    /// Unique identifier for the provider (e.g., "telegram")
    fn id(&self) -> &str;

    /// Check if the provider is enabled and configured
    fn is_enabled(&self) -> bool;

    /// Send a simple text notification
    async fn send_text(&self, message: &str) -> Result<(), NotificationError>;

    /// Send a notification with an image (e.g., poster)
    /// Should fallback to text if image sending fails or is not supported
    async fn send_image(&self, message: &str, image_url: &str) -> Result<(), NotificationError>;
}
```

### 2. Error Types

```rust
// crates/server/src/services/notification/error.rs

#[derive(Debug, thiserror::Error)]
pub enum NotificationError {
    #[error("Provider not configured: {0}")]
    NotConfigured(String),

    #[error("Failed to send notification: {0}")]
    SendFailed(String),

    #[error("HTTP request failed: {0}")]
    Http(#[from] reqwest::Error),
}
```

### 3. Provider Implementations

#### TelegramProvider

```rust
// crates/server/src/services/notification/telegram.rs

pub struct TelegramProvider {
    client: Client,  // From HttpClientService
    config: TelegramConfig,
}

impl TelegramProvider {
    pub fn new(client: Client, config: TelegramConfig) -> Self {
        Self { client, config }
    }
}

#[async_trait]
impl NotificationProvider for TelegramProvider {
    fn id(&self) -> &str { "telegram" }

    fn is_enabled(&self) -> bool {
        self.config.enabled
            && !self.config.bot_token.is_empty()
            && !self.config.chat_id.is_empty()
    }

    async fn send_text(&self, message: &str) -> Result<(), NotificationError> {
        if !self.is_enabled() {
            return Err(NotificationError::NotConfigured("telegram".into()));
        }

        let url = format!(
            "https://api.telegram.org/bot{}/sendMessage",
            self.config.bot_token
        );

        let resp = self.client
            .post(&url)
            .json(&serde_json::json!({
                "chat_id": self.config.chat_id,
                "text": message,
                "parse_mode": "HTML"
            }))
            .send()
            .await?;

        if !resp.status().is_success() {
            let error_text = resp.text().await.unwrap_or_default();
            return Err(NotificationError::SendFailed(error_text));
        }

        Ok(())
    }

    async fn send_image(&self, message: &str, image_url: &str) -> Result<(), NotificationError> {
        if !self.is_enabled() {
            return Err(NotificationError::NotConfigured("telegram".into()));
        }

        let url = format!(
            "https://api.telegram.org/bot{}/sendPhoto",
            self.config.bot_token
        );

        let resp = self.client
            .post(&url)
            .json(&serde_json::json!({
                "chat_id": self.config.chat_id,
                "photo": image_url,
                "caption": message,
                "parse_mode": "HTML"
            }))
            .send()
            .await?;

        if !resp.status().is_success() {
            // Fallback to text-only on image failure
            tracing::warn!("Failed to send image, falling back to text");
            return self.send_text(message).await;
        }

        Ok(())
    }
}
```

### 4. NotificationService

Manages the lifecycle of providers and broadcasts notifications. Subscribes to `SettingsService` for dynamic configuration updates.

```rust
// crates/server/src/services/notification.rs

pub struct NotificationService {
    telegram: Arc<RwLock<Option<TelegramProvider>>>,
    settings: Arc<SettingsService>,
    http_client: Arc<HttpClientService>,
}

impl NotificationService {
    pub fn new(
        settings: Arc<SettingsService>,
        http_client: Arc<HttpClientService>,
    ) -> Self {
        let telegram = Arc::new(RwLock::new(None));

        // Initialize provider
        let initial_settings = settings.get();
        if let Some(provider) = Self::create_telegram_provider(
            &initial_settings.notification.telegram,
            &http_client,
        ) {
            *telegram.blocking_write() = Some(provider);
        }

        // Watch for settings changes
        let telegram_clone = Arc::clone(&telegram);
        let http_clone = Arc::clone(&http_client);
        let mut watcher = settings.subscribe();

        tokio::spawn(async move {
            loop {
                if watcher.changed().await.is_err() {
                    break;
                }
                let new_settings = watcher.borrow().notification.telegram.clone();
                let new_provider = Self::create_telegram_provider(&new_settings, &http_clone);
                *telegram_clone.write().await = new_provider;
            }
        });

        Self {
            telegram,
            settings,
            http_client,
        }
    }

    fn create_telegram_provider(
        config: &TelegramConfig,
        http_client: &HttpClientService,
    ) -> Option<TelegramProvider> {
        if config.enabled && !config.bot_token.is_empty() {
            Some(TelegramProvider::new(http_client.get_client(), config.clone()))
        } else {
            None
        }
    }

    pub async fn send(&self, message: &str, image_url: Option<&str>) -> Result<(), NotificationError> {
        let settings = self.settings.get();
        if !settings.notification.enabled {
            return Ok(())
        }

        let telegram = self.telegram.read().await;
        if let Some(provider) = telegram.as_ref() {
            if provider.is_enabled() {
                if let Some(url) = image_url {
                    provider.send_image(message, url).await?;
                } else {
                    provider.send_text(message).await?;
                }
            }
        }

        Ok(())
    }
}
```

### 5. NotificationJob

A scheduler job that periodically checks for completed downloads and triggers notifications.

```rust
// crates/server/src/services/scheduler/notification_job.rs

pub struct NotificationJob {
    db: SqlitePool,
    downloader: Arc<DownloaderService>,
    notification: Arc<NotificationService>,
    settings: Arc<SettingsService>,
}

impl NotificationJob {
    pub fn new(
        db: SqlitePool,
        downloader: Arc<DownloaderService>,
        notification: Arc<NotificationService>,
        settings: Arc<SettingsService>,
    ) -> Self {
        Self { db, downloader, notification, settings }
    }

    /// Get bangumi info for a task via info_hash -> torrent -> bangumi
    async fn get_bangumi_for_task(&self, task: &Task) -> Option<Bangumi> {
        // task.id is the info_hash
        let torrent = TorrentRepository::get_by_info_hash(&self.db, &task.id)
            .await
            .ok()? // Use ? operator for cleaner error handling
            ?;
        BangumiRepository::get_by_id(&self.db, torrent.bangumi_id)
            .await
            .ok()? // Use ? operator for cleaner error handling
    }

    /// Format notification message
    /// Note: Use a simple escape function or inline escaping for HTML special chars
    fn format_message(&self, task: &Task, bangumi: Option<&Bangumi>) -> String {
        let size = Self::format_size(task.total_size);

        if let Some(b) = bangumi {
            format!(
                "<b>Download Complete</b>\n\n{}\nSize: {}",
                Self::escape_html(&b.title_chinese),
                size
            )
        } else {
            format!(
                "<b>Download Complete</b>\n\n{}\nSize: {}",
                Self::escape_html(&task.name),
                size
            )
        }
    }

    /// Escape HTML special characters for Telegram
    fn escape_html(s: &str) -> String {
        s.replace('&', "&amp;")
            .replace('<', "&lt;")
            .replace('>', "&gt;")
    }

    fn format_size(bytes: i64) -> String {
        const KB: i64 = 1024;
        const MB: i64 = KB * 1024;
        const GB: i64 = MB * 1024;

        if bytes >= GB {
            format!("{:.2} GB", bytes as f64 / GB as f64)
        } else if bytes >= MB {
            format!("{:.2} MB", bytes as f64 / MB as f64)
        } else if bytes >= KB {
            format!("{:.2} KB", bytes as f64 / KB as f64)
        } else {
            format!("{} B", bytes)
        }
    }
}

#[async_trait]
impl SchedulerJob for NotificationJob {
    fn name(&self) -> &'static str {
        "Notification"
    }

    fn interval(&self) -> Duration {
        Duration::from_secs(30)
    }

    async fn execute(&self) -> JobResult {
        // 1. Check if notification is enabled
        let settings = self.settings.get();
        if !settings.notification.enabled {
            return Ok(())
        }

        // 2. Check if downloader is available
        if !self.downloader.is_available().await {
            tracing::debug!("Downloader not available, skipping notification check");
            return Ok(())
        }

        // 3. Get completed tasks managed by moe (with "moe" tag)
        let filter = TaskFilter::new().tag("moe");
        let tasks = match self.downloader.get_tasks(Some(&filter)).await {
            Ok(tasks) => tasks,
            Err(e) => {
                tracing::error!("Failed to get tasks: {}", e);
                return Ok(())
            }
        };

        // 4. Filter completed tasks without "notified" tag
        let completed: Vec<_> = tasks
            .into_iter()
            .filter(|t| t.is_completed() && !t.has_tag("notified"))
            .collect();

        if completed.is_empty() {
            return Ok(())
        }

        tracing::info!("Found {} completed tasks to notify", completed.len());

        // 5. Send notifications
        for task in completed {
            let bangumi = self.get_bangumi_for_task(&task).await;
            let message = self.format_message(&task, bangumi.as_ref());
            let image_url = bangumi
                .as_ref()
                .and_then(|b| b.poster_url.as_deref())
                .filter(|url| url.starts_with("http"));

            // Send notification
            if let Err(e) = self.notification.send(&message, image_url).await {
                tracing::error!("Failed to send notification for {}: {}", task.name, e);
                continue; // Don't mark as notified, retry next time
            }

            // 6. Mark as notified
            if let Err(e) = self.downloader.add_tags(&task.id, &["notified"]).await {
                tracing::error!("Failed to add notified tag to {}: {}", task.name, e);
            }
        }

        Ok(())
    }
}
```

## Configuration

### TOML Format

```toml
[notification]
enabled = true

[notification.telegram]
enabled = true
bot_token = "123456:ABC-DEF1234ghIkl-zyx57W2v1u123ew11"
chat_id = "123456789"
```

### Rust Structs

```rust
// crates/server/src/models/settings.rs

/// Notification configuration
#[derive(Debug, Clone, Serialize, Deserialize, ToSchema)]
pub struct NotificationSettings {
    /// Global enable/disable for notifications
    #[serde(default)]
    pub enabled: bool,

    /// Telegram configuration
    #[serde(default)]
    pub telegram: TelegramConfig,
}

impl Default for NotificationSettings {
    fn default() -> Self {
        Self {
            enabled: false,
            telegram: TelegramConfig::default(),
        }
    }
}

#[derive(Debug, Clone, Default, Serialize, Deserialize, ToSchema)]
pub struct TelegramConfig {
    #[serde(default)]
    pub enabled: bool,

    #[serde(default)]
    pub bot_token: String,

    #[serde(default)]
    pub chat_id: String,
}

// Update structs for partial updates
#[derive(Debug, Clone, Default, Deserialize, ToSchema)]
pub struct UpdateNotificationSettings {
    pub enabled: Option<bool>,
    pub telegram: Option<UpdateTelegramConfig>,
}

#[derive(Debug, Clone, Default, Deserialize, ToSchema)]
pub struct UpdateTelegramConfig {
    pub enabled: Option<bool>,

    #[serde(default)]
    #[schema(value_type = Option<String>)]
    pub bot_token: Clearable<String>,

    #[serde(default)]
    #[schema(value_type = Option<String>)]
    pub chat_id: Clearable<String>,
}
```

## AppState Integration

```rust
// crates/server/src/state.rs

pub struct AppState {
    // ... existing fields
    pub notification: Arc<NotificationService>,
}

impl AppState {
    pub async fn new(db: SqlitePool, config: Config, settings: SettingsService) -> Self {
        // ... existing initialization

        // Create NotificationService
        let notification = Arc::new(NotificationService::new(
            Arc::clone(&settings),
            Arc::clone(&http_client_service),
        ));

        // Register NotificationJob in scheduler
        let scheduler = SchedulerService::new()
            .with_arc_job(Arc::clone(&rss_fetch_job))
            .with_job(LogCleanupJob::new(Arc::clone(&logs)))
            .with_job(NotificationJob::new(
                db.clone(),
                Arc::clone(&downloader_arc),
                Arc::clone(&notification),
                Arc::clone(&settings),
            ));

        scheduler.start();

        // ...
    }
}
```

## Database Changes

No schema changes are required. The system uses:
- Existing `torrent` table (has `bangumi_id` foreign key) to link downloads to bangumi
- Downloader's tag mechanism (`notified` tag) to track notification state

## File Structure

```
crates/server/src/
├── models/
│   └── settings.rs              # Add NotificationSettings, TelegramConfig
├── services/
│   ├── notification.rs          # NotificationService (new)
│   ├── notification/
│   │   ├── mod.rs               # Module exports
│   │   ├── traits.rs            # NotificationProvider trait
│   │   ├── error.rs             # NotificationError
│   │   └── telegram.rs          # TelegramProvider
│   ├── scheduler/
│   │   └── notification_job.rs  # NotificationJob (new)
│   └── scheduler.rs             # Export notification_job
└── state.rs                     # Integrate NotificationService
```

## Edge Cases & Safety

- **Duplicate Notifications**: Handled by the `notified` tag. If the tag fails to apply, notification will retry on next run (acceptable trade-off).
- **Image Failures**: TelegramProvider falls back to text-only if image sending fails.
- **Downloader Unavailable**: Job skips execution if downloader is not configured or unreachable.
- **Settings Changes**: NotificationService subscribes to SettingsService and dynamically rebuilds providers when configuration changes.
- **Only "moe" Tasks**: Only notifies for tasks with the "moe" tag (tasks added by this system), ignoring manually added torrents.

```