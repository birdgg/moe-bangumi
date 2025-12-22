use crate::client::QBittorrentClient;
use crate::error::QBittorrentError;

impl QBittorrentClient {
    /// Login to qBittorrent WebUI
    /// POST /api/v2/auth/login
    pub async fn login(&self, username: &str, password: &str) -> crate::Result<()> {
        let url = self.url("/auth/login");
        let params = [("username", username), ("password", password)];

        let response = self.client().post(&url).form(&params).send().await?;

        let status = response.status();
        let body = response.text().await.unwrap_or_default();

        if status.is_success() && body == "Ok." {
            tracing::debug!("Successfully logged in to qBittorrent");
            Ok(())
        } else if body == "Fails." {
            Err(QBittorrentError::Auth("Invalid username or password".into()))
        } else {
            Err(QBittorrentError::Auth(format!(
                "Login failed: {} - {}",
                status.as_u16(),
                body
            )))
        }
    }

    /// Logout from qBittorrent WebUI
    /// POST /api/v2/auth/logout
    pub async fn logout(&self) -> crate::Result<()> {
        let url = self.url("/auth/logout");
        let response = self.client().post(&url).send().await?;
        self.handle_response(response).await
    }
}
