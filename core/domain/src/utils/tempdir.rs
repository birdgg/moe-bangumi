//! Temporary download directory utilities
//!
//! Provides functions for generating and detecting temporary download directories.
//! Files are downloaded to a `.downloading` directory and moved to their final
//! location after download completes.

use std::path::Path;

/// The name of the temporary download directory
const TEMP_DIR_NAME: &str = ".downloading";

/// Generate temporary download directory path.
///
/// Creates a `.downloading` directory at the parent level of save_path,
/// preserving the final folder name.
///
/// # Examples
///
/// ```
/// use domain::utils::generate_temp_download_dir;
///
/// let save_path = "/Media/Bangumi/Title (2024)/Season 01";
/// let temp_path = generate_temp_download_dir(save_path);
/// assert_eq!(temp_path, "/Media/Bangumi/.downloading/Title (2024)/Season 01");
/// ```
pub fn generate_temp_download_dir(save_path: &str) -> String {
    let path = Path::new(save_path);

    // We need to find the base path (e.g., /Media/Bangumi) and the relative part
    // The pattern is: {base_path}/{title folder}/{season folder}
    // We want: {base_path}/.downloading/{title folder}/{season folder}

    let components: Vec<_> = path.components().collect();

    // Need at least 2 components to insert .downloading before the last two
    // e.g., /Media/Bangumi/Title/Season -> /Media/Bangumi/.downloading/Title/Season
    if components.len() >= 3 {
        // Find where to insert .downloading (before the bangumi folder)
        // Typically: /Media/Bangumi/Title (2024)/Season 01
        // Insert .downloading after /Media/Bangumi

        // Find the index - we want to insert after the base path
        // For /Media/Bangumi/Title/Season, we insert after index 2 (Bangumi)
        // This means components[0..3] = [/, Media, Bangumi], insert at index 3

        // Actually, let's keep it simple: insert .downloading before the last 2 components
        let insert_pos = components.len() - 2;

        let base: std::path::PathBuf = components[..insert_pos].iter().collect();
        let relative: std::path::PathBuf = components[insert_pos..].iter().collect();

        base.join(TEMP_DIR_NAME)
            .join(relative)
            .to_string_lossy()
            .into_owned()
    } else if components.len() == 2 {
        // Only base + one folder, insert .downloading before the last component
        let base: std::path::PathBuf = components[..1].iter().collect();
        let relative: std::path::PathBuf = components[1..].iter().collect();

        base.join(TEMP_DIR_NAME)
            .join(relative)
            .to_string_lossy()
            .into_owned()
    } else {
        // Too few components, just append .downloading
        path.join(TEMP_DIR_NAME).to_string_lossy().into_owned()
    }
}

/// Check if a path is in the temporary download directory.
///
/// Uses path component analysis for accurate detection across platforms.
///
/// # Examples
///
/// ```
/// use domain::utils::is_temp_download_path;
///
/// assert!(is_temp_download_path("/Media/Bangumi/.downloading/Title/Season 01"));
/// assert!(!is_temp_download_path("/Media/Bangumi/Title/Season 01"));
/// ```
pub fn is_temp_download_path(path: &str) -> bool {
    let path_obj = Path::new(path);
    path_obj.components().any(|c| {
        if let std::path::Component::Normal(os_str) = c {
            os_str == TEMP_DIR_NAME
        } else {
            false
        }
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_generate_temp_download_dir() {
        // Standard case: /Media/Bangumi/Title (2024)/Season 01
        assert_eq!(
            generate_temp_download_dir("/Media/Bangumi/Title (2024)/Season 01"),
            "/Media/Bangumi/.downloading/Title (2024)/Season 01"
        );

        // With tmdb id
        assert_eq!(
            generate_temp_download_dir("/Media/Bangumi/Title (2024) {tmdb-12345}/Season 01"),
            "/Media/Bangumi/.downloading/Title (2024) {tmdb-12345}/Season 01"
        );

        // Different base path
        assert_eq!(
            generate_temp_download_dir("/downloads/anime/Show/Season 02"),
            "/downloads/anime/.downloading/Show/Season 02"
        );
    }

    #[test]
    fn test_generate_temp_download_dir_short_paths() {
        // Only two components
        assert_eq!(
            generate_temp_download_dir("/downloads/show"),
            "/.downloading/downloads/show"
        );
    }

    #[test]
    fn test_is_temp_download_path() {
        // Positive cases
        assert!(is_temp_download_path(
            "/Media/Bangumi/.downloading/Title/Season 01"
        ));
        assert!(is_temp_download_path("/downloads/.downloading/show"));
        assert!(is_temp_download_path("/a/b/.downloading/c/d/e.mkv"));
        assert!(is_temp_download_path("/.downloading/foo")); // Root level temp dir
        assert!(is_temp_download_path("/path/to/.downloading")); // Temp dir at end

        // Negative cases
        assert!(!is_temp_download_path("/Media/Bangumi/Title/Season 01"));
        assert!(!is_temp_download_path("/downloads/show"));
        assert!(!is_temp_download_path("/path/.downloadingmore/")); // Not exact match
        assert!(!is_temp_download_path("/path/not.downloading/file")); // Different name
    }
}
