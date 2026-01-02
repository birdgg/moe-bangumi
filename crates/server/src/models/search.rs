use serde::{Deserialize, Serialize};
use utoipa::ToSchema;

/// Parsed BGM.tv subject with extracted season info
#[derive(Debug, Clone, Serialize, Deserialize, ToSchema)]
pub struct ParsedSubject {
    pub id: i64,
    /// Original name (Japanese)
    pub name: String,
    /// Chinese name
    pub name_cn: String,
    /// Parsed Chinese name (without season info)
    pub parsed_name: String,
    /// Parsed season number (defaults to 1)
    pub season: i32,
    /// Release date
    pub date: String,
    /// Platform (TV, Movie, OVA)
    pub platform: String,
    /// Poster image URL
    pub image: String,
    /// Total episodes
    pub eps: i64,
}

impl ParsedSubject {
    pub fn from_bgmtv(subject: bgmtv::Subject) -> Self {
        let parsed = parser::bgmtv::parse_bgmtv_name(&subject.name_cn);
        Self {
            id: subject.id,
            name: subject.name,
            name_cn: subject.name_cn,
            parsed_name: parsed.name,
            season: parsed.season,
            date: subject.date,
            platform: subject.platform,
            image: subject.image,
            eps: subject.eps,
        }
    }
}
