//! Calendar seed data generator
//!
//! This script fetches historical calendar data from Mikan and BGM.tv,
//! then saves it as a JSON file for seeding the database on first startup.
//!
//! Usage: cargo run --bin calendar_seed

use std::sync::Arc;
use std::time::Duration;

use bgmtv::BgmtvClient;
use futures::stream::{self, StreamExt};
use mikan::{MikanClient, Season};
use server::models::{CalendarSeedData, CalendarSeedEntry, SeasonData};
use server::SeasonIterator;
use tracing_subscriber::EnvFilter;

const OUTPUT_PATH: &str = "assets/seed/calendar.json";
const REQUEST_DELAY_SECS: u64 = 60;
const END_YEAR: i32 = 2013;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Initialize logging
    tracing_subscriber::fmt()
        .with_env_filter(EnvFilter::new("info"))
        .init();

    tracing::info!("Starting calendar seed data generation");
    tracing::info!("Target: {} Winter to current season", END_YEAR);
    tracing::info!("Request delay: {} seconds between seasons", REQUEST_DELAY_SECS);

    // Create HTTP client
    let http_client = reqwest::Client::builder()
        .user_agent("birdgg/moe-bangumi")
        .timeout(Duration::from_secs(30))
        .build()?;

    // Create API clients
    let mikan = Arc::new(MikanClient::new(http_client.clone()));
    let bgmtv = Arc::new(BgmtvClient::with_client(http_client));

    let mut all_seasons = Vec::new();
    let iterator = SeasonIterator::from_current_to(END_YEAR, Season::Winter);
    let seasons: Vec<_> = iterator.collect();
    let total = seasons.len();

    tracing::info!("Will process {} seasons", total);

    for (idx, (year, season)) in seasons.into_iter().enumerate() {
        let season_str = season.to_db_string();
        tracing::info!(
            "[{}/{}] Processing {} {}...",
            idx + 1,
            total,
            year,
            season_str
        );

        match fetch_season_data(&mikan, &bgmtv, year, season).await {
            Ok(data) => {
                tracing::info!(
                    "[{}/{}] Got {} entries for {} {}",
                    idx + 1,
                    total,
                    data.entries.len(),
                    year,
                    season_str
                );
                all_seasons.push(data);
            }
            Err(e) => {
                tracing::warn!(
                    "[{}/{}] Failed to fetch {} {}: {}",
                    idx + 1,
                    total,
                    year,
                    season_str,
                    e
                );
            }
        }

        // Delay between seasons (skip delay after last one)
        if idx + 1 < total {
            tracing::info!("Waiting {} seconds before next request...", REQUEST_DELAY_SECS);
            tokio::time::sleep(Duration::from_secs(REQUEST_DELAY_SECS)).await;
        }
    }

    // Create output directory if needed
    std::fs::create_dir_all("assets/seed")?;

    // Save to JSON file
    let seed_data = CalendarSeedData { seasons: all_seasons };
    let json = serde_json::to_string_pretty(&seed_data)?;
    std::fs::write(OUTPUT_PATH, &json)?;

    let total_entries: usize = seed_data.seasons.iter().map(|s| s.entries.len()).sum();
    tracing::info!(
        "Seed data saved to {} ({} seasons, {} total entries)",
        OUTPUT_PATH,
        seed_data.seasons.len(),
        total_entries
    );

    Ok(())
}

/// Fetch calendar data for a single season
async fn fetch_season_data(
    mikan: &Arc<MikanClient>,
    bgmtv: &Arc<BgmtvClient>,
    year: i32,
    season: Season,
) -> Result<SeasonData, Box<dyn std::error::Error>> {
    let season_str = season.to_db_string().to_string();

    // Step 1: Get seasonal bangumi list from Mikan
    let seasonal_list = mikan.get_seasonal_bangumi_list(year, season).await?;
    tracing::debug!("Fetched {} bangumi from Mikan", seasonal_list.len());

    if seasonal_list.is_empty() {
        return Ok(SeasonData {
            year,
            season: season_str,
            entries: Vec::new(),
        });
    }

    // Step 2: Fetch BGM.tv IDs (with concurrency limit)
    let mikan_clone = Arc::clone(mikan);
    let items: Vec<_> = seasonal_list
        .iter()
        .map(|b| {
            let mikan_id = b.mikan_id.clone();
            let air_week = b.air_week;
            let poster_url = b.poster_url.clone();
            let name = b.name.clone();
            let mikan = Arc::clone(&mikan_clone);
            (mikan_id, air_week, poster_url, name, mikan)
        })
        .collect();

    let bgmtv_mappings: Vec<_> = stream::iter(items)
        .map(|(mikan_id, air_week, poster_url, name, mikan)| async move {
            match mikan.get_bangumi_bgmtv_id(&mikan_id).await {
                Ok(Some(bgmtv_id)) => Some((mikan_id, bgmtv_id, air_week, poster_url, name)),
                Ok(None) => {
                    tracing::debug!("No BGM.tv ID for mikan_id: {}", mikan_id);
                    None
                }
                Err(e) => {
                    tracing::debug!("Failed to get BGM.tv ID for {}: {}", mikan_id, e);
                    None
                }
            }
        })
        .buffer_unordered(5)
        .filter_map(|x| async { x })
        .collect()
        .await;

    tracing::debug!(
        "Got {} BGM.tv IDs out of {} bangumi",
        bgmtv_mappings.len(),
        seasonal_list.len()
    );

    if bgmtv_mappings.is_empty() {
        return Ok(SeasonData {
            year,
            season: season_str,
            entries: Vec::new(),
        });
    }

    // Step 3: Fetch BGM.tv subject details
    let bgmtv_clone = Arc::clone(bgmtv);
    let tasks: Vec<_> = bgmtv_mappings
        .into_iter()
        .map(|(mikan_id, bgmtv_id, air_week, poster_url, name)| {
            let bgmtv = Arc::clone(&bgmtv_clone);
            (mikan_id, bgmtv_id, air_week, poster_url, name, bgmtv)
        })
        .collect();

    let entries: Vec<CalendarSeedEntry> = stream::iter(tasks)
        .map(|(mikan_id, bgmtv_id, air_week, poster_url, _name, bgmtv)| async move {
            match bgmtv.get_subject(bgmtv_id).await {
                Ok(subject) => {
                    // Parse year from date
                    let subject_year = subject
                        .date
                        .as_ref()
                        .and_then(|date| date.get(0..4))
                        .and_then(|s| s.parse().ok())
                        .unwrap_or(year);

                    // Parse platform
                    let platform = subject
                        .platform
                        .as_ref()
                        .map(|p| match p.to_lowercase().as_str() {
                            "movie" | "劇場版" => "movie",
                            "ova" => "ova",
                            _ => "tv",
                        })
                        .unwrap_or("tv")
                        .to_string();

                    // Use Mikan poster_url, fallback to BGM.tv
                    let final_poster_url = poster_url.or(Some(subject.images.large.clone()));

                    Some(CalendarSeedEntry {
                        mikan_id,
                        bgmtv_id,
                        title_chinese: if subject.name_cn.is_empty() {
                            subject.name.clone()
                        } else {
                            subject.name_cn.clone()
                        },
                        title_japanese: Some(subject.name).filter(|s| !s.is_empty()),
                        air_week,
                        poster_url: final_poster_url,
                        year: subject_year,
                        platform,
                        total_episodes: subject.total_episodes as i32,
                        air_date: subject.date,
                    })
                }
                Err(e) => {
                    tracing::debug!("Failed to fetch BGM.tv subject {}: {}", bgmtv_id, e);
                    None
                }
            }
        })
        .buffer_unordered(10)
        .filter_map(|x| async { x })
        .collect()
        .await;

    Ok(SeasonData {
        year,
        season: season_str,
        entries,
    })
}
