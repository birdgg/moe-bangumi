import { z } from "zod";

/**
 * Downloader types supported by the system
 */
export const downloaderTypes = ["qBittorrent", "Transmission"] as const;
export type DownloaderTypeValue = (typeof downloaderTypes)[number];

/**
 * qBittorrent configuration schema
 */
export const qbittorrentConfigSchema = z.object({
  url: z.string(),
  username: z.string(),
  password: z.string(),
});

/**
 * Transmission configuration schema
 */
export const transmissionConfigSchema = z.object({
  url: z.string(),
  username: z.string(),
  password: z.string(),
});

/**
 * Per-downloader configurations schema
 */
export const downloaderConfigsSchema = z.object({
  qbittorrent: qbittorrentConfigSchema,
  transmission: transmissionConfigSchema,
});

/**
 * Downloader settings schema with per-type configs
 */
export const downloaderSchema = z.object({
  type: z.enum(downloaderTypes),
  save_path: z.string(),
  configs: downloaderConfigsSchema,
});

/**
 * Filter configuration schema
 */
export const filterSchema = z.object({
  global_rss_filters: z.array(z.string()),
});

/**
 * Proxy configuration schema
 */
export const proxySchema = z.object({
  url: z.string(),
  username: z.string(),
  password: z.string(),
});

/**
 * Telegram configuration schema
 */
export const telegramConfigSchema = z.object({
  bot_token: z.string(),
  chat_id: z.string(),
});

/**
 * Notification settings schema
 */
export const notificationSchema = z.object({
  telegram: telegramConfigSchema,
});

/**
 * Complete settings form schema
 */
export const settingsFormSchema = z.object({
  downloader: downloaderSchema,
  filter: filterSchema,
  proxy: proxySchema,
  notification: notificationSchema,
});

/**
 * TypeScript types inferred from schemas
 */
export type QBittorrentConfigFormData = z.infer<typeof qbittorrentConfigSchema>;
export type TransmissionConfigFormData = z.infer<typeof transmissionConfigSchema>;
export type DownloaderConfigsFormData = z.infer<typeof downloaderConfigsSchema>;
export type DownloaderFormData = z.infer<typeof downloaderSchema>;
export type FilterFormData = z.infer<typeof filterSchema>;
export type ProxyFormData = z.infer<typeof proxySchema>;
export type TelegramConfigFormData = z.infer<typeof telegramConfigSchema>;
export type NotificationFormData = z.infer<typeof notificationSchema>;
export type SettingsFormData = z.infer<typeof settingsFormSchema>;
