use axum::{
    extract::{Query, State},
    Json,
};
use bgmtv::CalendarDay;
use mikan::Season;
use serde::Deserialize;
use utoipa::IntoParams;

use crate::{error::AppResult, services::CalendarService, state::AppState};

/// Query parameters for calendar endpoints
#[derive(Debug, Deserialize, IntoParams)]
pub struct CalendarQuery {
    /// Year (e.g., 2025). Defaults to current year.
    pub year: Option<i32>,
    /// Season: winter, spring, summer, fall. Defaults to current season.
    pub season: Option<Season>,
}

impl CalendarQuery {
    /// Get year and season, defaulting to current if not specified
    fn resolve(&self) -> (i32, Season) {
        let (current_year, current_season) = CalendarService::current_season();
        (
            self.year.unwrap_or(current_year),
            self.season.unwrap_or(current_season),
        )
    }
}

/// Get calendar (weekly anime schedule)
///
/// Returns cached data from database. If the database is empty for the requested season,
/// automatically fetches from Mikan/BGM.tv and populates the database.
/// Defaults to current season if year/season not specified.
#[utoipa::path(
    get,
    path = "/api/calendar",
    tag = "calendar",
    params(CalendarQuery),
    responses(
        (status = 200, description = "Weekly anime schedule", body = Vec<CalendarDay>)
    )
)]
pub async fn get_calendar(
    State(state): State<AppState>,
    Query(query): Query<CalendarQuery>,
) -> AppResult<Json<Vec<CalendarDay>>> {
    let (year, season) = query.resolve();

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
/// Defaults to current season if year/season not specified.
#[utoipa::path(
    post,
    path = "/api/calendar/refresh",
    tag = "calendar",
    params(CalendarQuery),
    responses(
        (status = 200, description = "Calendar refreshed successfully", body = Vec<CalendarDay>)
    )
)]
pub async fn refresh_calendar(
    State(state): State<AppState>,
    Query(query): Query<CalendarQuery>,
) -> AppResult<Json<Vec<CalendarDay>>> {
    let (year, season) = query.resolve();
    tracing::info!("Manual calendar refresh requested for {} {:?}", year, season);
    state.calendar.refresh_calendar(year, season).await?;
    let calendar = state.calendar.get_calendar(year, season).await?;
    Ok(Json(calendar))
}
