use axum::{
    extract::{Path, State},
    http::StatusCode,
    response::IntoResponse,
    Json,
};

use crate::state::AppState;

/// Get episodes by subject ID from BGM.tv
#[utoipa::path(
    get,
    path = "/api/episodes/{subject_id}",
    tag = "episodes",
    params(
        ("subject_id" = i64, Path, description = "BGM.tv subject ID")
    ),
    responses(
        (status = 200, description = "Episodes list", body = Vec<bgmtv::Episode>),
        (status = 500, description = "Internal server error")
    )
)]
pub async fn get_episodes(
    State(state): State<AppState>,
    Path(subject_id): Path<i64>,
) -> impl IntoResponse {
    match state.bgmtv.get_episodes(subject_id).await {
        Ok(response) => (StatusCode::OK, Json(response.data)).into_response(),
        Err(e) => {
            tracing::error!("Failed to get episodes for subject {}: {}", subject_id, e);
            (StatusCode::INTERNAL_SERVER_ERROR, "Internal server error").into_response()
        }
    }
}
