use utoipa::OpenApi;

use crate::models::{
    BangumiWithRss, DownloaderSettings, FilterSettings, Log, LogLevel, Rss, Settings,
    UpdateBangumiRequest, UpdateDownloaderSettings, UpdateFilterSettings, UpdateSettings,
};

#[derive(OpenApi)]
#[openapi(
    info(
        title = "Moe API",
        version = "1.0.0"
    ),
    tags(
        (name = "search", description = "Bangumi search endpoints"),
        (name = "bangumi", description = "Bangumi management endpoints"),
        (name = "settings", description = "Application settings endpoints"),
        (name = "logs", description = "System logs endpoints")
    ),
    components(schemas(
        bgmtv::SearchSubjectsResponse,
        bgmtv::Subject,
        BangumiWithRss,
        Rss,
        UpdateBangumiRequest,
        Settings,
        DownloaderSettings,
        FilterSettings,
        UpdateSettings,
        UpdateDownloaderSettings,
        UpdateFilterSettings,
        Log,
        LogLevel
    ))
)]
pub struct ApiDoc;
