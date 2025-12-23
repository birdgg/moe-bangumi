use server::Environment;
use std::env;
use std::net::SocketAddr;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    dotenvy::dotenv().ok();

    tracing_subscriber::fmt()
        .with_env_filter(
            tracing_subscriber::EnvFilter::from_default_env()
                .add_directive(tracing::Level::INFO.into()),
        )
        .init();

    let app_env = Environment::from_str(&env::var("APP_ENV").unwrap_or_default());
    let port: u16 = env::var("PORT")
        .unwrap_or_else(|_| "3000".to_string())
        .parse()?;
    let data_path = env::var("DATA_PATH")
        .unwrap_or_else(|_| app_env.default_data_path().to_string_lossy().to_string());
    let tmdb_api_key = env::var("TMDB_API_KEY").expect("TMDB_API_KEY must be set");

    let addr: SocketAddr = format!("0.0.0.0:{}", port).parse()?;

    server::run_server(addr, app_env, &data_path, &tmdb_api_key).await
}
