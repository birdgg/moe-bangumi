use serde::{Deserialize, Serialize};
use utoipa::ToSchema;

#[derive(Debug, Clone, Serialize, Deserialize, ToSchema)]
pub struct SearchResult {
    pub id: String,
    pub name: String,
}

#[derive(Debug, Clone, Serialize, Deserialize, ToSchema)]
pub struct BangumiDetail {
    pub subgroups: Vec<Subgroup>,
}

#[derive(Debug, Clone, Serialize, Deserialize, ToSchema)]
pub struct Subgroup {
    pub id: String,
    pub name: String,
    pub rss_url: String,
    pub episodes: Vec<Episode>,
}

#[derive(Debug, Clone, Serialize, Deserialize, ToSchema)]
pub struct Episode {
    pub name: String,
    pub torrent_url: Option<String>,
}
