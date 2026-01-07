use std::path::PathBuf;
use std::sync::Arc;
use std::time::Duration;

use bgmtv::BgmtvClient;
use metadata::{BgmtvProvider, TmdbProvider};
use mikan::MikanClient;
use parking_lot::RwLock;
use rss::RssClient;
use sqlx::SqlitePool;
use tmdb::TmdbClient;
use updater::{UpdateConfig, UpdateService, UpdateServiceHandle};

use crate::config::Config;
use domain::services::actors::metadata::{
    create_metadata_actor, MetadataHandle, MetadataService, PosterService,
};
use domain::services::{
    create_downloader_service, create_notification_service, BangumiService, CacheService,
    CalendarService, DownloaderHandle, HttpClientService, LogService, NotificationService,
    RenameService, RssProcessingService, SettingsService, WashingService,
};
use jobs::{
    create_log_cleanup_actor, create_rename_actor, create_rss_fetch_actor, LogCleanupHandle,
    RenameHandle, RssFetchHandle,
};

/// Infrastructure layer - core dependencies
#[derive(Clone)]
pub struct AppInfra {
    pub db: SqlitePool,
    pub config: Arc<Config>,
    pub http_client: Arc<HttpClientService>,
    pub settings: Arc<SettingsService>,
}

/// API clients layer - external service clients
#[derive(Clone)]
pub struct AppClients {
    pub mikan: Arc<MikanClient>,
    pub rss: Arc<RssClient>,
}

/// Business services layer - core application services
#[derive(Clone)]
pub struct AppServices {
    pub metadata: Arc<MetadataService>,
    pub bangumi: Arc<BangumiService>,
    pub calendar: Arc<CalendarService>,
    pub cache: Arc<CacheService>,
    pub logs: Arc<LogService>,
    pub downloader: Arc<DownloaderHandle>,
    pub poster: Arc<PosterService>,
    pub notification: Arc<NotificationService>,
    pub rename: Arc<RenameService>,
    pub update: UpdateServiceHandle,
}

/// Background actors layer - actor handles
#[derive(Clone)]
pub struct AppActors {
    pub metadata: Arc<MetadataHandle>,
    #[allow(dead_code)]
    pub rss_fetch: RssFetchHandle,
    #[allow(dead_code)]
    pub rename: RenameHandle,
    #[allow(dead_code)]
    pub log_cleanup: LogCleanupHandle,
}

/// Application state - organized into logical groups
#[derive(Clone)]
pub struct AppState {
    pub infra: Arc<AppInfra>,
    pub clients: Arc<AppClients>,
    pub services: AppServices,
    pub actors: AppActors,
}

/// Create a client provider closure from HttpClientService
fn create_client_provider(
    http_service: Arc<HttpClientService>,
) -> Arc<
    dyn Fn() -> std::pin::Pin<
            Box<
                dyn std::future::Future<
                        Output = Result<reqwest::Client, Box<dyn std::error::Error + Send + Sync>>,
                    > + Send,
            >,
        > + Send
        + Sync,
> {
    Arc::new(move || {
        // get_client() is now sync, wrap in async for compatibility with ClientProvider type
        let client = http_service.get_client();
        Box::pin(async move { Ok(client) })
    })
}

impl AppState {
    pub fn new(
        db: SqlitePool,
        config: Config,
        settings: SettingsService,
        current_version: &str,
    ) -> Self {
        // Wrap settings in Arc first (needed by HttpClientService and DownloaderService)
        let settings = Arc::new(settings);

        // Create HTTP client service with dynamic proxy support
        let http_client = Arc::new(
            HttpClientService::new(Arc::clone(&settings))
                .expect("Failed to create HTTP client service"),
        );
        let client_provider = create_client_provider(Arc::clone(&http_client));

        // Create API clients
        let tmdb_api_key: tmdb::ApiKey =
            Arc::new(RwLock::new(settings.get().tmdb.api_key.clone()));
        let tmdb = Arc::new(TmdbClient::new(
            http_client.get_client(),
            Arc::clone(&tmdb_api_key),
        ));

        // Spawn background task to watch for TMDB API key changes
        let tmdb_api_key_clone = Arc::clone(&tmdb_api_key);
        let mut tmdb_watcher = settings.subscribe();
        tokio::spawn(async move {
            loop {
                if tmdb_watcher.changed().await.is_err() {
                    break;
                }
                let new_settings = tmdb_watcher.borrow().clone();
                let new_api_key = new_settings.tmdb.api_key;

                let should_update = {
                    let current = tmdb_api_key_clone.read();
                    *current != new_api_key
                };

                if should_update {
                    *tmdb_api_key_clone.write() = new_api_key;
                    tracing::info!("TMDB API key updated");
                }
            }
        });

        let bgmtv = Arc::new(BgmtvClient::new(http_client.get_client()));
        let mikan = Arc::new(MikanClient::with_client_provider(Arc::clone(
            &client_provider,
        )));
        let rss = Arc::new(RssClient::with_client_provider(Arc::clone(&client_provider)));

        // Create log service for logging and notifications
        let logs = Arc::new(LogService::new(db.clone()));

        // Create downloader service with settings reference (Actor mode)
        let downloader = Arc::new(create_downloader_service(Arc::clone(&settings)));

        // Create poster service with dynamic client provider
        let poster = Arc::new(PosterService::with_client_provider(
            Arc::clone(&client_provider),
            config.posters_path(),
        ));

        // Create unified metadata providers
        let bgmtv_provider = Arc::new(BgmtvProvider::new(Arc::clone(&bgmtv)));
        let tmdb_provider = Arc::new(TmdbProvider::new(Arc::clone(&tmdb)));

        // Create metadata service (using providers instead of clients)
        let metadata = Arc::new(MetadataService::new(
            db.clone(),
            Arc::clone(&bgmtv_provider),
            Arc::clone(&tmdb_provider),
        ));

        // Create metadata actor (handles poster downloads with internal scheduling)
        let metadata_actor = Arc::new(create_metadata_actor(db.clone(), Arc::clone(&poster)));

        // Create cache service
        let cache = Arc::new(CacheService::new(db.clone()));

        // Create washing service (for priority-based torrent replacement)
        let washing = Arc::new(WashingService::new(
            db.clone(),
            Arc::clone(&downloader),
            Arc::clone(&settings),
        ));

        // Create RSS processing service (shared by BangumiService and RssFetchJob)
        let rss_processing = Arc::new(RssProcessingService::new(
            db.clone(),
            Arc::clone(&rss),
            Arc::clone(&downloader),
            Arc::clone(&settings),
            Arc::clone(&washing),
        ));

        // Create bangumi service (with MetadataService and RSS processing for immediate fetch)
        let bangumi = Arc::new(BangumiService::new(
            db.clone(),
            Arc::clone(&metadata),
            Arc::clone(&rss_processing),
            Arc::clone(&settings),
        ));

        // Create notification service (Actor mode)
        let notification = Arc::new(create_notification_service(
            Arc::clone(&settings),
            Arc::clone(&http_client),
        ));

        // Wrap config in Arc for sharing
        let config = Arc::new(config);

        // Create rename service (for file renaming to Plex/Jellyfin format)
        let rename = Arc::new(RenameService::new(
            db.clone(),
            Arc::clone(&downloader),
            Arc::clone(&notification),
            Arc::clone(&config),
        ));

        // Create calendar service (fetches from Mikan -> BGM.tv)
        let calendar = Arc::new(CalendarService::new(
            db.clone(),
            Arc::clone(&metadata),
            Arc::clone(&mikan),
            Arc::clone(&metadata_actor),
        ));

        // Create update service (reuse HTTP client for proxy support)
        let update_config = UpdateConfig::new("birdgg", "moe-bangumi", current_version)
            .bin_name("moe")
            .target_dir(PathBuf::from("/data/bin"))
            .check_interval(Duration::from_secs(86400)) // 24 hours
            .auto_check(true);

        let (update_handle, update_service) =
            UpdateService::new(update_config, http_client.get_client());

        // Spawn update service actor
        tokio::spawn(update_service.run());

        // Start background actors
        // Note: These actors have internal timers and run independently
        let rss_fetch_actor = create_rss_fetch_actor(db.clone(), Arc::clone(&rss_processing));
        let log_cleanup_actor = create_log_cleanup_actor(Arc::clone(&logs));
        let rename_actor = create_rename_actor(Arc::clone(&rename));

        // Build sub-structures
        let infra = Arc::new(AppInfra {
            db,
            config,
            http_client,
            settings,
        });

        let clients = Arc::new(AppClients { mikan, rss });

        let services = AppServices {
            metadata,
            bangumi,
            calendar,
            cache,
            logs,
            downloader,
            poster,
            notification,
            rename,
            update: update_handle,
        };

        let actors = AppActors {
            metadata: metadata_actor,
            rss_fetch: rss_fetch_actor,
            rename: rename_actor,
            log_cleanup: log_cleanup_actor,
        };

        Self {
            infra,
            clients,
            services,
            actors,
        }
    }
}
