use axum::{
    extract::{Query, State},
    http::StatusCode,
    response::IntoResponse,
    Json,
};

use crate::repositories::CacheRepository;
use crate::state::AppState;
use tmdb::DiscoverBangumiParams;

use super::{SearchQuery, MIKAN_SEARCH_CACHE_TTL};

/// Search for bangumi (Japanese anime) on BGM.tv
#[utoipa::path(
    get,
    path = "/api/search/bgmtv",
    tag = "search",
    params(SearchQuery),
    responses(
        (status = 200, description = "Search results", body = Vec<bgmtv::Subject>),
        (status = 500, description = "Internal server error")
    )
)]
pub async fn search_bgmtv(
    State(state): State<AppState>,
    Query(query): Query<SearchQuery>,
) -> impl IntoResponse {
    match state.bgmtv.search_bangumi(&query.keyword).await {
        Ok(response) => (StatusCode::OK, Json(response.data)).into_response(),
        Err(e) => {
            tracing::error!("Failed to search bangumi: {}", e);
            (StatusCode::INTERNAL_SERVER_ERROR, "Internal server error").into_response()
        }
    }
}

/// Search for anime on TMDB using discover API
#[utoipa::path(
    get,
    path = "/api/search/tmdb",
    tag = "search",
    params(SearchQuery),
    responses(
        (status = 200, description = "Search results from TMDB", body = Vec<tmdb::models::TvShow>),
        (status = 500, description = "Internal server error")
    )
)]
pub async fn search_tmdb(
    State(state): State<AppState>,
    Query(query): Query<SearchQuery>,
) -> impl IntoResponse {
    let params = DiscoverBangumiParams {
        with_text_query: Some(query.keyword),
    };
    match state.tmdb.discover_bangumi(params).await {
        Ok(response) => (StatusCode::OK, Json(response.results)).into_response(),
        Err(e) => {
            tracing::error!("Failed to search TMDB: {}", e);
            (StatusCode::INTERNAL_SERVER_ERROR, "Internal server error").into_response()
        }
    }
}

/// Search for bangumi on Mikan
#[utoipa::path(
    get,
    path = "/api/search/mikan",
    tag = "search",
    params(SearchQuery),
    responses(
        (status = 200, description = "Search results from Mikan", body = Vec<mikan::SearchResult>),
        (status = 500, description = "Internal server error")
    )
)]
pub async fn search_mikan(
    State(state): State<AppState>,
    Query(query): Query<SearchQuery>,
) -> impl IntoResponse {
    let cache_key = format!("mikan:search:{}", query.keyword);

    // Try cache first
    if let Ok(Some(cached)) = CacheRepository::get::<Vec<mikan::SearchResult>>(
        &state.db,
        &cache_key,
        MIKAN_SEARCH_CACHE_TTL,
    )
    .await
    {
        return (StatusCode::OK, Json(cached)).into_response();
    }

    // Fetch from Mikan
    match state.mikan.search_bangumi(&query.keyword).await {
        Ok(results) => {
            // Cache the results
            if let Err(e) = CacheRepository::set(&state.db, &cache_key, &results).await {
                tracing::warn!("Failed to cache Mikan search results: {}", e);
            }
            (StatusCode::OK, Json(results)).into_response()
        }
        Err(e) => {
            tracing::error!("Failed to search Mikan: {}", e);
            (StatusCode::INTERNAL_SERVER_ERROR, "Internal server error").into_response()
        }
    }
}
