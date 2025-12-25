use reqwest::Client;

use crate::error::RssError;
use crate::models::{RssItem, RssSource};
use crate::parsers::{parse_mikan_feed, parse_nyaa_feed};

/// RSS feed fetcher client
pub struct RssClient {
    client: Client,
}

impl RssClient {
    /// Create a new RssClient with a custom reqwest Client
    pub fn with_client(client: Client) -> Self {
        Self { client }
    }

    /// Fetch and parse an RSS feed
    ///
    /// # Arguments
    /// * `source` - The RSS source to fetch from
    ///
    /// # Returns
    /// A vector of parsed RSS items
    ///
    /// # Example
    /// ```no_run
    /// use rss::{RssClient, RssSource};
    ///
    /// # async fn example() -> rss::Result<()> {
    /// let client = RssClient::new();
    /// let items = client.fetch(&RssSource::Mikan(
    ///     "https://mikanani.me/RSS/Bangumi?bangumiId=123".into()
    /// )).await?;
    ///
    /// for item in items {
    ///     println!("{}", item.title());
    /// }
    /// # Ok(())
    /// # }
    /// ```
    pub async fn fetch(&self, source: &RssSource) -> crate::Result<Vec<RssItem>> {
        let url = source.url();
        tracing::debug!("Fetching RSS feed from: {}", url);

        let response = self.client.get(url).send().await?;
        let status = response.status();

        if !status.is_success() {
            return Err(RssError::Parse(format!(
                "HTTP {} when fetching {}",
                status, url
            )));
        }

        let bytes = response.bytes().await?;

        let items: Vec<RssItem> = match source {
            RssSource::Mikan(_) => {
                let items = parse_mikan_feed(&bytes)?;
                items.into_iter().map(RssItem::Mikan).collect()
            }
            RssSource::Nyaa(_) => {
                let items = parse_nyaa_feed(&bytes)?;
                items.into_iter().map(RssItem::Nyaa).collect()
            }
        };

        tracing::debug!("Parsed {} items from RSS feed", items.len());
        Ok(items)
    }
}
