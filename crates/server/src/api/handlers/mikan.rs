use axum::{
    extract::{Query, State},
    http::StatusCode,
    response::IntoResponse,
    Json,
};

use crate::repositories::CacheRepository;
use crate::state::AppState;

use super::{IdQuery, MIKAN_DETAIL_CACHE_TTL};

/// Get bangumi detail with RSS URLs from Mikan
#[utoipa::path(
    get,
    path = "/api/mikan/rss",
    tag = "mikan",
    params(IdQuery),
    responses(
        (status = 200, description = "Bangumi detail with subgroups and RSS URLs", body = mikan::BangumiDetail),
        (status = 500, description = "Internal server error")
    )
)]
pub async fn get_mikan_rss(
    State(state): State<AppState>,
    Query(query): Query<IdQuery>,
) -> impl IntoResponse {
    let cache_key = format!("mikan:detail:{}", query.id);

    // Try cache first
    if let Ok(Some(cached)) =
        CacheRepository::get::<mikan::BangumiDetail>(&state.db, &cache_key, MIKAN_DETAIL_CACHE_TTL)
            .await
    {
        return (StatusCode::OK, Json(cached)).into_response();
    }

    // Fetch from Mikan
    match state.mikan.get_bangumi_detail(&query.id).await {
        Ok(detail) => {
            // Cache the results
            if let Err(e) = CacheRepository::set(&state.db, &cache_key, &detail).await {
                tracing::warn!("Failed to cache Mikan detail: {}", e);
            }
            (StatusCode::OK, Json(detail)).into_response()
        }
        Err(e) => {
            tracing::error!("Failed to get Mikan RSS for {}: {}", query.id, e);
            (StatusCode::INTERNAL_SERVER_ERROR, "Internal server error").into_response()
        }
    }
}
