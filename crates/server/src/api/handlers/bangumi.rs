use axum::{
    extract::{Path, State},
    http::StatusCode,
    response::IntoResponse,
    Json,
};

use crate::models::{
    Bangumi, BangumiWithRss, Clearable, CreateBangumi, CreateRss, UpdateBangumi,
    UpdateBangumiRequest,
};
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
                    is_primary: entry.is_primary,
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

/// Get a bangumi by ID with its RSS subscriptions
#[utoipa::path(
    get,
    path = "/api/bangumi/{id}",
    tag = "bangumi",
    params(
        ("id" = i64, Path, description = "Bangumi ID")
    ),
    responses(
        (status = 200, description = "Bangumi with RSS subscriptions", body = BangumiWithRss),
        (status = 404, description = "Bangumi not found"),
        (status = 500, description = "Internal server error")
    )
)]
pub async fn get_bangumi_by_id(
    State(state): State<AppState>,
    Path(id): Path<i64>,
) -> impl IntoResponse {
    match BangumiRepository::get_by_id(&state.db, id).await {
        Ok(Some(bangumi)) => {
            // Get RSS subscriptions for this bangumi
            match RssRepository::get_by_bangumi_id(&state.db, id).await {
                Ok(rss_entries) => {
                    let result = BangumiWithRss {
                        bangumi,
                        rss_entries,
                    };
                    (StatusCode::OK, Json(result)).into_response()
                }
                Err(e) => {
                    tracing::error!("Failed to get RSS for bangumi {}: {}", id, e);
                    (StatusCode::INTERNAL_SERVER_ERROR, "Internal server error").into_response()
                }
            }
        }
        Ok(None) => (StatusCode::NOT_FOUND, "Bangumi not found").into_response(),
        Err(e) => {
            tracing::error!("Failed to get bangumi {}: {}", id, e);
            (StatusCode::INTERNAL_SERVER_ERROR, "Internal server error").into_response()
        }
    }
}

/// Update a bangumi
#[utoipa::path(
    patch,
    path = "/api/bangumi/{id}",
    tag = "bangumi",
    params(
        ("id" = i64, Path, description = "Bangumi ID")
    ),
    request_body = UpdateBangumiRequest,
    responses(
        (status = 200, description = "Bangumi updated successfully", body = BangumiWithRss),
        (status = 404, description = "Bangumi not found"),
        (status = 500, description = "Internal server error")
    )
)]
pub async fn update_bangumi(
    State(state): State<AppState>,
    Path(id): Path<i64>,
    Json(payload): Json<UpdateBangumiRequest>,
) -> impl IntoResponse {
    // Check if bangumi exists
    match BangumiRepository::get_by_id(&state.db, id).await {
        Ok(Some(_)) => {}
        Ok(None) => return (StatusCode::NOT_FOUND, "Bangumi not found").into_response(),
        Err(e) => {
            tracing::error!("Failed to get bangumi {}: {}", id, e);
            return (StatusCode::INTERNAL_SERVER_ERROR, "Internal server error").into_response();
        }
    }

    // Build update data
    let update_data = UpdateBangumi {
        episode_offset: payload.episode_offset,
        auto_download: payload.auto_download,
        save_path: match payload.save_path {
            Some(Some(path)) => Clearable::Set(path),
            Some(None) => Clearable::Clear,
            None => Clearable::Unchanged,
        },
        ..Default::default()
    };

    // Update bangumi
    if let Err(e) = BangumiRepository::update(&state.db, id, update_data).await {
        tracing::error!("Failed to update bangumi {}: {}", id, e);
        return (StatusCode::INTERNAL_SERVER_ERROR, "Internal server error").into_response();
    }

    // Sync RSS entries if provided
    if let Some(rss_entries) = payload.rss_entries {
        // Delete existing RSS entries
        if let Err(e) = RssRepository::delete_by_bangumi_id(&state.db, id).await {
            tracing::error!("Failed to delete RSS for bangumi {}: {}", id, e);
            return (StatusCode::INTERNAL_SERVER_ERROR, "Internal server error").into_response();
        }

        // Create new RSS entries
        for entry in rss_entries {
            let create_rss = CreateRss {
                bangumi_id: id,
                url: entry.url,
                enabled: true,
                exclude_filters: entry.filters,
                is_primary: entry.is_primary,
            };

            if let Err(e) = RssRepository::create(&state.db, create_rss).await {
                tracing::error!("Failed to create RSS subscription: {}", e);
            }
        }
    }

    // Return updated bangumi with RSS
    match BangumiRepository::get_by_id(&state.db, id).await {
        Ok(Some(bangumi)) => match RssRepository::get_by_bangumi_id(&state.db, id).await {
            Ok(rss_entries) => {
                let result = BangumiWithRss {
                    bangumi,
                    rss_entries,
                };
                (StatusCode::OK, Json(result)).into_response()
            }
            Err(e) => {
                tracing::error!("Failed to get RSS for bangumi {}: {}", id, e);
                (StatusCode::INTERNAL_SERVER_ERROR, "Internal server error").into_response()
            }
        },
        Ok(None) => (StatusCode::NOT_FOUND, "Bangumi not found").into_response(),
        Err(e) => {
            tracing::error!("Failed to get bangumi {}: {}", id, e);
            (StatusCode::INTERNAL_SERVER_ERROR, "Internal server error").into_response()
        }
    }
}
