use reqwest::Client;
use std::path::PathBuf;
use thiserror::Error;

const TMDB_IMAGE_BASE_URL: &str = "https://image.tmdb.org/t/p/original";

/// Errors that can occur when downloading or managing posters.
#[derive(Debug, Error)]
pub enum PosterError {
    #[error("HTTP request failed: {0}")]
    Request(#[from] reqwest::Error),

    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),

    #[error("Invalid poster path: {0}")]
    InvalidPath(String),

    #[error("HTTP error: {0}")]
    HttpStatus(u16),
}

/// Service for downloading and managing poster images from TMDB.
///
/// Handles poster downloads with path validation, atomic file writes,
/// and automatic deduplication via local file caching.
pub struct PosterService {
    http_client: Client,
    posters_dir: PathBuf,
}

impl PosterService {
    /// Create a new PosterService.
    ///
    /// # Arguments
    /// * `http_client` - Shared reqwest client for HTTP requests
    /// * `posters_dir` - Directory to store downloaded posters
    pub fn new(http_client: Client, posters_dir: PathBuf) -> Self {
        Self {
            http_client,
            posters_dir,
        }
    }

    /// Build the local path string for a given filename.
    fn local_path_string(filename: &str) -> String {
        format!("/posters/{}", filename)
    }

    /// Validate and extract filename from a TMDB poster path.
    ///
    /// Returns the filename without the leading slash, or an error if invalid.
    fn validate_filename(poster_path: &str) -> Result<&str, PosterError> {
        let filename = poster_path.strip_prefix('/').ok_or_else(|| {
            PosterError::InvalidPath(format!(
                "Poster path must start with '/': {}",
                poster_path
            ))
        })?;

        // Prevent path traversal attacks
        if filename.contains("..") || filename.contains('/') {
            return Err(PosterError::InvalidPath(format!(
                "Invalid poster path: {}",
                poster_path
            )));
        }

        Ok(filename)
    }

    /// Extract TMDB poster path from various URL formats.
    ///
    /// Handles:
    /// - Full TMDB URL: "https://image.tmdb.org/t/p/w500/abc.jpg" -> "/abc.jpg"
    /// - TMDB path: "/abc.jpg" -> "/abc.jpg"
    /// - Local path: "/posters/abc.jpg" -> None (already local)
    fn extract_tmdb_path(url: &str) -> Option<String> {
        if url.starts_with("/posters/") {
            return None;
        }

        if url.contains("image.tmdb.org") {
            // URL format: https://image.tmdb.org/t/p/{size}/{path}
            url.rsplit_once("/t/p/")?
                .1
                .split_once('/')
                .map(|(_, path)| format!("/{}", path))
        } else if url.starts_with('/') {
            Some(url.to_string())
        } else {
            None
        }
    }

    /// Download a poster from TMDB and return the local path.
    ///
    /// # Arguments
    /// * `poster_path` - TMDB poster path (e.g., "/kXE4mzog.jpg")
    ///
    /// # Returns
    /// * `Ok(String)` - Local path relative to data dir (e.g., "/posters/kXE4mzog.jpg")
    /// * `Err` - Download or save failed
    pub async fn download_poster(&self, poster_path: &str) -> Result<String, PosterError> {
        let filename = Self::validate_filename(poster_path)?;
        let local_path = self.posters_dir.join(filename);

        // Check if file already exists (avoid repeat download)
        if local_path.exists() {
            tracing::debug!("Poster already exists: {}", local_path.display());
            return Ok(Self::local_path_string(filename));
        }

        // Build TMDB URL and download
        let url = format!("{}{}", TMDB_IMAGE_BASE_URL, poster_path);
        tracing::info!("Downloading poster: {}", poster_path);

        let response = self.http_client.get(&url).send().await?;

        if !response.status().is_success() {
            return Err(PosterError::HttpStatus(response.status().as_u16()));
        }

        let bytes = response.bytes().await?;

        // Ensure directory exists
        if let Some(parent) = local_path.parent() {
            tokio::fs::create_dir_all(parent).await?;
        }

        // Atomic write: use unique temp file to avoid race conditions
        let temp_suffix = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .map(|d| d.as_nanos())
            .unwrap_or(0);
        let tmp_path = local_path.with_extension(format!("tmp.{}", temp_suffix));

        tokio::fs::write(&tmp_path, &bytes).await?;

        // Rename to final path (atomic on most filesystems)
        if let Err(e) = tokio::fs::rename(&tmp_path, &local_path).await {
            // Clean up temp file on failure
            let _ = tokio::fs::remove_file(&tmp_path).await;
            return Err(e.into());
        }

        tracing::info!("Saved poster: {}", local_path.display());
        Ok(Self::local_path_string(filename))
    }

    /// Try to download poster from a URL.
    ///
    /// Handles different URL formats:
    /// - TMDB full URL: "https://image.tmdb.org/t/p/w500/abc.jpg"
    /// - TMDB path: "/abc.jpg"
    /// - Local path: "/posters/abc.jpg" (returns as-is)
    ///
    /// # Returns
    /// * `Some(local_path)` - Successfully downloaded or already local
    /// * `None` - Not a TMDB URL or download failed
    pub async fn try_download(&self, url: &str) -> Option<String> {
        // Already a local path
        if url.starts_with("/posters/") {
            tracing::debug!("URL is already a local path: {}", url);
            return Some(url.to_string());
        }

        let poster_path = Self::extract_tmdb_path(url)?;

        match self.download_poster(&poster_path).await {
            Ok(local_path) => Some(local_path),
            Err(e) => {
                tracing::warn!("Failed to download poster: {}", e);
                None
            }
        }
    }
}
