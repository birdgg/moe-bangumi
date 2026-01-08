//! Scan API handlers
//!
//! Provides endpoints for scanning directories and importing bangumi.

use axum::{extract::State, http::StatusCode, Json};
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::sync::Arc;
#[cfg(feature = "openapi")]
use utoipa::ToSchema;

use crate::infra::error::{AppError, AppResult};
use crate::state::AppState;

/// Request body for scan import
#[derive(Debug, Default, Deserialize)]
#[cfg_attr(feature = "openapi", derive(ToSchema))]
pub struct ScanImportRequest {
    // Reserved for future options (e.g., dry_run, specific path)
}

/// Response for scan import
#[derive(Debug, Serialize)]
#[cfg_attr(feature = "openapi", derive(ToSchema))]
pub struct ScanImportResponse {
    /// Status message
    pub message: String,
    /// Timestamp when scan was started
    pub started_at: DateTime<Utc>,
}

/// Start a background scan and import task
///
/// Scans the downloader save path for Plex/Jellyfin formatted directories
/// and imports them as bangumi subscriptions.
#[cfg_attr(feature = "openapi", utoipa::path(
    post,
    path = "/api/scan/import",
    request_body = ScanImportRequest,
    responses(
        (status = 202, description = "Scan started in background", body = ScanImportResponse),
        (status = 409, description = "Scan already in progress"),
    ),
    tag = "scan"
))]
pub async fn scan_import(
    State(state): State<AppState>,
    Json(_payload): Json<ScanImportRequest>,
) -> AppResult<(StatusCode, Json<ScanImportResponse>)> {
    let scan_service = Arc::clone(&state.services.scan);

    // Check if a scan is already in progress
    if !scan_service.try_start_scan() {
        return Err(AppError::conflict("A scan is already in progress"));
    }

    // Start background task
    tokio::spawn(async move {
        let result = scan_service.scan_and_import().await;

        tracing::info!(
            "Background scan completed: {} scanned, {} imported, {} skipped, {} failed",
            result.total_scanned,
            result.imported,
            result.skipped,
            result.failed
        );
    });

    Ok((
        StatusCode::ACCEPTED,
        Json(ScanImportResponse {
            message: "Scan started in background".to_string(),
            started_at: Utc::now(),
        }),
    ))
}
