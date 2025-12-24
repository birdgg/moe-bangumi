use axum::{extract::State, http::StatusCode, response::IntoResponse, Json};

use crate::models::{Bangumi, CreateBangumi, CreateRss};
use crate::repositories::{BangumiRepository, RssRepository};
use crate::state::AppState;

/// Create a new bangumi
#[utoipa::path(
    post,
    path = "/api/bangumi",
    tag = "bangumi",
    request_body = CreateBangumi,
    responses(
        (status = 201, description = "Bangumi created successfully", body = Bangumi),
        (status = 500, description = "Internal server error")
    )
)]
pub async fn create_bangumi(
    State(state): State<AppState>,
    Json(mut payload): Json<CreateBangumi>,
) -> impl IntoResponse {
    // Extract RSS entries before creating bangumi
    let rss_entries = payload.rss_entries.clone();

    // Try to download poster from TMDB if available
    if let Some(ref poster_url) = payload.poster_url {
        if let Some(local_path) = state.poster.try_download(poster_url).await {
            payload.poster_url = Some(local_path);
        }
    }

    match BangumiRepository::create(&state.db, payload).await {
        Ok(bangumi) => {
            // Create RSS subscriptions for the new bangumi
            for entry in rss_entries {
                let create_rss = CreateRss {
                    bangumi_id: bangumi.id,
                    url: entry.url,
                    enabled: true,
                    exclude_filters: entry.filters,
                };

                if let Err(e) = RssRepository::create(&state.db, create_rss).await {
                    tracing::error!("Failed to create RSS subscription: {}", e);
                }
            }

            (StatusCode::CREATED, Json(bangumi)).into_response()
        }
        Err(e) => {
            tracing::error!("Failed to create bangumi: {}", e);
            (StatusCode::INTERNAL_SERVER_ERROR, "Internal server error").into_response()
        }
    }
}

/// Get all bangumi
#[utoipa::path(
    get,
    path = "/api/bangumi",
    tag = "bangumi",
    responses(
        (status = 200, description = "List of all bangumi", body = Vec<Bangumi>),
        (status = 500, description = "Internal server error")
    )
)]
pub async fn get_bangumi(State(state): State<AppState>) -> impl IntoResponse {
    match BangumiRepository::get_all(&state.db).await {
        Ok(bangumi_list) => (StatusCode::OK, Json(bangumi_list)).into_response(),
        Err(e) => {
            tracing::error!("Failed to get all bangumi: {}", e);
            (StatusCode::INTERNAL_SERVER_ERROR, "Internal server error").into_response()
        }
    }
}
