use axum::{extract::State, http::StatusCode, response::IntoResponse, Json};

use crate::models::{Settings, UpdateSettings};
use crate::state::AppState;

/// Get application settings
#[utoipa::path(
    get,
    path = "/api/settings",
    tag = "settings",
    responses(
        (status = 200, description = "Application settings", body = Settings),
        (status = 500, description = "Internal server error")
    )
)]
pub async fn get_settings(State(state): State<AppState>) -> impl IntoResponse {
    let settings = state.settings.get();
    (StatusCode::OK, Json(settings)).into_response()
}

/// Update application settings
#[utoipa::path(
    patch,
    path = "/api/settings",
    tag = "settings",
    request_body = UpdateSettings,
    responses(
        (status = 200, description = "Settings updated successfully", body = Settings),
        (status = 500, description = "Internal server error")
    )
)]
pub async fn update_settings(
    State(state): State<AppState>,
    Json(payload): Json<UpdateSettings>,
) -> impl IntoResponse {
    match state.settings.update(payload).await {
        Ok(settings) => (StatusCode::OK, Json(settings)).into_response(),
        Err(e) => {
            tracing::error!("Failed to update settings: {}", e);
            (StatusCode::INTERNAL_SERVER_ERROR, "Internal server error").into_response()
        }
    }
}

/// Reset settings to defaults
#[utoipa::path(
    post,
    path = "/api/settings/reset",
    tag = "settings",
    responses(
        (status = 200, description = "Settings reset successfully", body = Settings),
        (status = 500, description = "Internal server error")
    )
)]
pub async fn reset_settings(State(state): State<AppState>) -> impl IntoResponse {
    match state.settings.reset().await {
        Ok(settings) => (StatusCode::OK, Json(settings)).into_response(),
        Err(e) => {
            tracing::error!("Failed to reset settings: {}", e);
            (StatusCode::INTERNAL_SERVER_ERROR, "Internal server error").into_response()
        }
    }
}
