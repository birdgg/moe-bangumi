use axum::{
    extract::{Path, Query, State},
    http::StatusCode,
    response::IntoResponse,
    Json,
};
use serde::Deserialize;
use utoipa::IntoParams;

use crate::models::{Bangumi, CreateBangumi};
use crate::repositories::BangumiRepository;
use crate::state::AppState;
use tmdb::DiscoverBangumiParams;

/// Query parameters for keyword search
#[derive(Debug, Deserialize, IntoParams)]
pub struct SearchQuery {
    /// Keyword to search
    pub keyword: String,
}

/// Query parameters for ID lookup
#[derive(Debug, Deserialize, IntoParams)]
pub struct IdQuery {
    /// ID to lookup
    pub id: String,
}

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
    Json(payload): Json<CreateBangumi>,
) -> impl IntoResponse {
    match BangumiRepository::create(&state.db, payload).await {
        Ok(bangumi) => (StatusCode::CREATED, Json(bangumi)).into_response(),
        Err(e) => {
            tracing::error!("Failed to create bangumi: {}", e);
            (StatusCode::INTERNAL_SERVER_ERROR, "Internal server error").into_response()
        }
    }
}

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
    match state.mikan.search_bangumi(&query.keyword).await {
        Ok(results) => (StatusCode::OK, Json(results)).into_response(),
        Err(e) => {
            tracing::error!("Failed to search Mikan: {}", e);
            (StatusCode::INTERNAL_SERVER_ERROR, "Internal server error").into_response()
        }
    }
}

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
    match state.mikan.get_bangumi_detail(&query.id).await {
        Ok(detail) => (StatusCode::OK, Json(detail)).into_response(),
        Err(e) => {
            tracing::error!("Failed to get Mikan RSS for {}: {}", query.id, e);
            (StatusCode::INTERNAL_SERVER_ERROR, "Internal server error").into_response()
        }
    }
}
