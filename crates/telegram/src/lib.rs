//! Telegram Bot API client library.
//!
//! This crate provides a simple client for sending messages via Telegram Bot API.
//!
//! # Example
//!
//! ```rust,ignore
//! use telegram::TelegramClient;
//!
//! let client = TelegramClient::new(reqwest::Client::new(), "BOT_TOKEN", "CHAT_ID");
//! client.send_message("Hello, World!").await?;
//! ```

mod client;
mod error;

pub use client::TelegramClient;
pub use error::TelegramError;
