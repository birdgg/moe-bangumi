use axum::{extract::State, Json};
use bgmtv::CalendarDay;

use crate::{error::AppResult, services::CalendarService, state::AppState};

/// Get calendar (weekly anime schedule for current season)
///
/// Returns cached data from database. If the database is empty,
/// automatically fetches from Mikan/BGM.tv and populates the database.
#[utoipa::path(
    get,
    path = "/api/calendar",
    tag = "calendar",
    responses(
        (status = 200, description = "Weekly anime schedule", body = Vec<CalendarDay>)
    )
)]
pub async fn get_calendar(State(state): State<AppState>) -> AppResult<Json<Vec<CalendarDay>>> {
    let (year, season) = CalendarService::current_season();

    // Try to get from database first
    let calendar = state.calendar.get_calendar(year, season).await?;

    // If database is empty, fetch from API and cache it
    if calendar.is_empty() {
        tracing::info!(
            "Calendar database is empty for {} {:?}, fetching from Mikan/BGM.tv",
            year,
            season
        );
        state.calendar.refresh_calendar(year, season).await?;
        let calendar = state.calendar.get_calendar(year, season).await?;
        return Ok(Json(calendar));
    }

    Ok(Json(calendar))
}

/// Refresh calendar data
///
/// Forces a refresh of the calendar data from Mikan/BGM.tv API.
/// Returns the updated calendar data.
#[utoipa::path(
    post,
    path = "/api/calendar/refresh",
    tag = "calendar",
    responses(
        (status = 200, description = "Calendar refreshed successfully", body = Vec<CalendarDay>)
    )
)]
pub async fn refresh_calendar(State(state): State<AppState>) -> AppResult<Json<Vec<CalendarDay>>> {
    let (year, season) = CalendarService::current_season();
    tracing::info!("Manual calendar refresh requested for {} {:?}", year, season);
    state.calendar.refresh_calendar(year, season).await?;
    let calendar = state.calendar.get_calendar(year, season).await?;
    Ok(Json(calendar))
}
