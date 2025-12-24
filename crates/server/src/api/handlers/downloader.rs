use axum::{http::StatusCode, response::IntoResponse, Json};
use serde::Deserialize;

use crate::services::{Downloader, DownloaderClient, DownloaderConfig, DownloaderType};

/// Request body for testing downloader connection
#[derive(Debug, Deserialize, utoipa::ToSchema)]
pub struct TestDownloaderRequest {
    /// Downloader type (e.g., "qbittorrent")
    #[serde(rename = "type")]
    pub downloader_type: DownloaderType,
    /// Downloader Web UI URL
    pub url: String,
    /// Username
    pub username: String,
    /// Password
    pub password: String,
}

/// Test downloader connection with provided credentials
#[utoipa::path(
    post,
    path = "/api/downloader/test",
    tag = "downloader",
    request_body = TestDownloaderRequest,
    responses(
        (status = 200, description = "Connection successful"),
        (status = 401, description = "Authentication failed"),
        (status = 500, description = "Connection error")
    )
)]
pub async fn test_downloader_connection(
    Json(payload): Json<TestDownloaderRequest>,
) -> impl IntoResponse {
    let config = DownloaderConfig {
        downloader_type: payload.downloader_type,
        url: payload.url,
        username: Some(payload.username),
        password: Some(payload.password),
    };

    let client = match DownloaderClient::from_config(config) {
        Ok(c) => c,
        Err(e) => {
            tracing::error!("Failed to create downloader client: {}", e);
            return (StatusCode::INTERNAL_SERVER_ERROR, "Failed to create downloader client")
                .into_response();
        }
    };

    match client.authenticate().await {
        Ok(()) => {
            tracing::info!("Downloader connection test successful");
            (StatusCode::OK, "Connection successful").into_response()
        }
        Err(e) => {
            tracing::error!("Downloader connection test failed: {}", e);
            (StatusCode::UNAUTHORIZED, format!("Connection failed: {}", e)).into_response()
        }
    }
}
