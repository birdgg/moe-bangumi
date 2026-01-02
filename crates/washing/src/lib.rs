//! Priority-based torrent washing algorithm library.
//!
//! This crate provides a priority-based system for selecting the best torrent
//! among multiple options for the same episode. Priority is determined by:
//! 1. Subtitle group (highest weight)
//! 2. Subtitle language
//!
//! # Example
//!
//! ```
//! use washing::{PriorityConfig, PriorityCalculator, ComparableTorrent};
//! use parser::SubType;
//!
//! let config = PriorityConfig {
//!     subtitle_groups: vec!["ANi".to_string(), "LoliHouse".to_string()],
//!     subtitle_languages: vec![SubType::Chs, SubType::Cht],
//! };
//!
//! let calculator = PriorityCalculator::new(config);
//!
//! let torrent = ComparableTorrent {
//!     subtitle_group: Some("ANi".to_string()),
//!     subtitle_languages: vec![SubType::Chs, SubType::Jpn],
//! };
//!
//! let score = calculator.calculate_score(&torrent);
//! assert_eq!(score.group_rank, 0); // Highest priority
//! ```

use std::cmp::Ordering;

pub use parser::SubType;

/// Priority configuration from settings
#[derive(Debug, Clone, Default)]
pub struct PriorityConfig {
    /// Subtitle groups in priority order (first = highest priority)
    pub subtitle_groups: Vec<String>,
    /// Subtitle languages in priority order (first = highest priority)
    pub subtitle_languages: Vec<SubType>,
}

/// Priority score for comparison (lower rank = higher priority)
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PriorityScore {
    /// Subtitle group rank (0 = highest, usize::MAX = not configured/unknown)
    pub group_rank: usize,
    /// Subtitle language rank
    pub language_rank: usize,
}

impl PriorityScore {
    /// Create a score with all ranks set to lowest priority
    pub fn lowest() -> Self {
        Self {
            group_rank: usize::MAX,
            language_rank: usize::MAX,
        }
    }

    /// Check if this score has any configured priority (not all MAX)
    pub fn has_any_priority(&self) -> bool {
        self.group_rank != usize::MAX || self.language_rank != usize::MAX
    }
}

impl Ord for PriorityScore {
    fn cmp(&self, other: &Self) -> Ordering {
        // Compare in order: group > language
        // Lower rank = higher priority, so we compare in reverse
        match self.group_rank.cmp(&other.group_rank) {
            Ordering::Equal => self.language_rank.cmp(&other.language_rank),
            other => other,
        }
    }
}

impl PartialOrd for PriorityScore {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

/// Comparable torrent information extracted from parse result
#[derive(Debug, Clone, Default)]
pub struct ComparableTorrent {
    /// Subtitle group name
    pub subtitle_group: Option<String>,
    /// Subtitle languages/types (e.g., [Chs, Jpn])
    pub subtitle_languages: Vec<SubType>,
}

/// Priority calculator for comparing torrents
pub struct PriorityCalculator {
    config: PriorityConfig,
}

impl PriorityCalculator {
    /// Create a new priority calculator with the given configuration
    pub fn new(config: PriorityConfig) -> Self {
        Self { config }
    }

    /// Calculate priority score for a torrent
    pub fn calculate_score(&self, torrent: &ComparableTorrent) -> PriorityScore {
        PriorityScore {
            group_rank: self.get_group_rank(&torrent.subtitle_group),
            language_rank: self.get_language_rank(&torrent.subtitle_languages),
        }
    }

    /// Get exact match rank for subtitle group
    fn get_group_rank(&self, value: &Option<String>) -> usize {
        match value {
            Some(v) => self
                .config
                .subtitle_groups
                .iter()
                .position(|configured| configured == v)
                .unwrap_or(usize::MAX),
            None => usize::MAX,
        }
    }

    /// Get best matching rank for subtitle languages
    /// Returns the best (lowest) rank among all languages in the torrent
    fn get_language_rank(&self, languages: &[SubType]) -> usize {
        if languages.is_empty() {
            return usize::MAX;
        }

        // Find the best (lowest) rank among all languages
        languages
            .iter()
            .filter_map(|lang| {
                self.config
                    .subtitle_languages
                    .iter()
                    .position(|configured| configured == lang)
            })
            .min()
            .unwrap_or(usize::MAX)
    }

    /// Check if new torrent has higher priority than existing
    ///
    /// Returns true if `new` should replace `existing`
    pub fn is_higher_priority(
        &self,
        new: &ComparableTorrent,
        existing: &ComparableTorrent,
    ) -> bool {
        let new_score = self.calculate_score(new);
        let existing_score = self.calculate_score(existing);

        // Lower score = higher priority
        new_score < existing_score
    }

    /// Find the torrent with highest priority from a list
    ///
    /// Returns the torrent with the best (lowest) priority score
    pub fn find_best<'a>(&self, torrents: &'a [ComparableTorrent]) -> Option<&'a ComparableTorrent> {
        torrents
            .iter()
            .min_by_key(|t| self.calculate_score(t))
    }

    /// Find the best torrent and its score from a list
    pub fn find_best_with_score<'a>(
        &self,
        torrents: &'a [ComparableTorrent],
    ) -> Option<(&'a ComparableTorrent, PriorityScore)> {
        torrents
            .iter()
            .map(|t| (t, self.calculate_score(t)))
            .min_by(|(_, a), (_, b)| a.cmp(b))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn create_test_config() -> PriorityConfig {
        PriorityConfig {
            subtitle_groups: vec![
                "ANi".to_string(),
                "喵萌奶茶屋".to_string(),
                "桜都字幕组".to_string(),
            ],
            subtitle_languages: vec![SubType::Chs, SubType::Cht, SubType::Jpn],
        }
    }

    #[test]
    fn test_priority_score_ordering() {
        let score1 = PriorityScore {
            group_rank: 0,
            language_rank: 0,
        };
        let score2 = PriorityScore {
            group_rank: 1,
            language_rank: 0,
        };
        let score3 = PriorityScore {
            group_rank: 0,
            language_rank: 1,
        };

        // Lower rank = higher priority = smaller in ordering
        assert!(score1 < score2);
        assert!(score1 < score3);
        assert!(score3 < score2); // group_rank takes precedence
    }

    #[test]
    fn test_calculate_score() {
        let config = create_test_config();
        let calculator = PriorityCalculator::new(config);

        let torrent = ComparableTorrent {
            subtitle_group: Some("ANi".to_string()),
            subtitle_languages: vec![SubType::Chs],
        };

        let score = calculator.calculate_score(&torrent);
        assert_eq!(score.group_rank, 0);
        assert_eq!(score.language_rank, 0);
    }

    #[test]
    fn test_unknown_values_get_max_rank() {
        let config = create_test_config();
        let calculator = PriorityCalculator::new(config);

        let torrent = ComparableTorrent {
            subtitle_group: Some("未知字幕组".to_string()),
            subtitle_languages: vec![],
        };

        let score = calculator.calculate_score(&torrent);
        assert_eq!(score.group_rank, usize::MAX);
        assert_eq!(score.language_rank, usize::MAX);
    }

    #[test]
    fn test_is_higher_priority() {
        let config = create_test_config();
        let calculator = PriorityCalculator::new(config);

        let torrent_ani = ComparableTorrent {
            subtitle_group: Some("ANi".to_string()),
            subtitle_languages: vec![SubType::Chs],
        };

        let torrent_other = ComparableTorrent {
            subtitle_group: Some("喵萌奶茶屋".to_string()),
            subtitle_languages: vec![SubType::Chs],
        };

        // ANi has higher priority than 喵萌奶茶屋
        assert!(calculator.is_higher_priority(&torrent_ani, &torrent_other));
        assert!(!calculator.is_higher_priority(&torrent_other, &torrent_ani));
    }

    #[test]
    fn test_find_best() {
        let config = create_test_config();
        let calculator = PriorityCalculator::new(config);

        let torrents = vec![
            ComparableTorrent {
                subtitle_group: Some("桜都字幕组".to_string()),
                subtitle_languages: vec![SubType::Cht],
            },
            ComparableTorrent {
                subtitle_group: Some("ANi".to_string()),
                subtitle_languages: vec![SubType::Chs, SubType::Jpn],
            },
            ComparableTorrent {
                subtitle_group: Some("喵萌奶茶屋".to_string()),
                subtitle_languages: vec![SubType::Jpn],
            },
        ];

        let best = calculator.find_best(&torrents).unwrap();
        // ANi should be best because group has highest weight
        assert_eq!(best.subtitle_group, Some("ANi".to_string()));
    }

    #[test]
    fn test_multi_language_best_rank() {
        let config = create_test_config();
        let calculator = PriorityCalculator::new(config);

        // Torrent with multiple languages - should match best one
        let torrent = ComparableTorrent {
            subtitle_group: Some("ANi".to_string()),
            subtitle_languages: vec![SubType::Cht, SubType::Jpn], // Cht is rank 1, Jpn is rank 2
        };

        let score = calculator.calculate_score(&torrent);
        // Should get the best (lowest) rank among languages
        assert_eq!(score.language_rank, 1); // Cht is at position 1

        // Test with Chs included - should be rank 0
        let torrent2 = ComparableTorrent {
            subtitle_group: None,
            subtitle_languages: vec![SubType::Jpn, SubType::Chs], // Chs is rank 0
        };
        let score2 = calculator.calculate_score(&torrent2);
        assert_eq!(score2.language_rank, 0);
    }

    #[test]
    fn test_unknown_language_not_in_config() {
        let config = create_test_config();
        let calculator = PriorityCalculator::new(config);

        // Torrent with Eng which is not in config
        let torrent = ComparableTorrent {
            subtitle_group: Some("ANi".to_string()),
            subtitle_languages: vec![SubType::Eng],
        };

        let score = calculator.calculate_score(&torrent);
        assert_eq!(score.language_rank, usize::MAX);
    }
}
