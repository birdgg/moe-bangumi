use axum::{extract::State, Json};
use bgmtv::CalendarDay;

use crate::{error::AppResult, state::AppState};

// Cache TTL for calendar data (1 hour)
const CALENDAR_CACHE_TTL: i64 = 3600;

/// Get BGM.tv calendar (weekly anime schedule)
#[utoipa::path(
    get,
    path = "/api/calendar",
    tag = "calendar",
    responses(
        (status = 200, description = "Weekly anime schedule", body = Vec<CalendarDay>)
    )
)]
pub async fn get_calendar(State(state): State<AppState>) -> AppResult<Json<Vec<CalendarDay>>> {
    let cache_key = "bgmtv:calendar";
    let bgmtv = state.bgmtv.clone();

    let calendar = state
        .cache
        .get_or_fetch(cache_key, CALENDAR_CACHE_TTL, || async move {
            bgmtv.get_calendar().await
        })
        .await?;

    Ok(Json(calendar))
}
