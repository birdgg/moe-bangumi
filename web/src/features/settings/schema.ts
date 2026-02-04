import { z } from "zod";

export const subtitleLanguages = ["CHS", "CHT", "JAP", "ENG"] as const;
export type SubtitleLanguage = (typeof subtitleLanguages)[number];

export type SubtitlePattern = SubtitleLanguage[];

export const defaultSubtitlePatterns: SubtitlePattern[] = [
  ["CHS", "JAP"],
  ["CHT", "JAP"],
  ["CHS"],
  ["CHT"],
];

export function subtitlePatternLabel(pattern: SubtitlePattern): string {
  const sorted = [...pattern].sort();
  const key = sorted.join("+");
  const labels: Record<string, string> = {
    "CHS": "简",
    "CHT": "繁",
    "CHS+JAP": "简日",
    "CHT+JAP": "繁日",
    "JAP": "日",
    "ENG": "英",
  };
  return labels[key] ?? pattern.join("+");
}

export function patternsEqual(a: SubtitlePattern, b: SubtitlePattern): boolean {
  if (a.length !== b.length) return false;
  const sortedA = [...a].sort();
  const sortedB = [...b].sort();
  return sortedA.every((v, i) => v === sortedB[i]);
}

export function patternKey(pattern: SubtitlePattern): string {
  return [...pattern].sort().join("+");
}

export function mergeWithDefaultPatterns(saved: SubtitlePattern[]): SubtitlePattern[] {
  const result: SubtitlePattern[] = [];
  const usedKeys = new Set<string>();

  for (const pattern of saved) {
    const key = patternKey(pattern);
    const defaultPattern = defaultSubtitlePatterns.find(p => patternKey(p) === key);
    if (defaultPattern && !usedKeys.has(key)) {
      result.push(defaultPattern);
      usedKeys.add(key);
    }
  }

  for (const pattern of defaultSubtitlePatterns) {
    const key = patternKey(pattern);
    if (!usedKeys.has(key)) {
      result.push(pattern);
      usedKeys.add(key);
    }
  }

  return result;
}

export const downloaderSchema = z.object({
  url: z.string(),
  username: z.string(),
  password: z.string(),
  savePath: z.string(),
});

export const filterSchema = z.object({
  globalRssFilters: z.array(z.string()),
});

export const telegramConfigSchema = z.object({
  botToken: z.string(),
  chatId: z.string(),
});

export const notificationSchema = z.object({
  telegram: telegramConfigSchema,
});

export const groupSchema = z.object({
  name: z.string(),
  aliases: z.array(z.string()),
});

export const prioritySchema = z.object({
  groups: z.array(groupSchema),
  languages: z.array(z.array(z.enum(subtitleLanguages))),
});

export const tmdbSchema = z.object({
  apiKey: z.string(),
});

export const embySchema = z.object({
  url: z.string(),
  apiKey: z.string(),
});

export const aiSchema = z.object({
  apiKey: z.string(),
  model: z.string(),
  confidenceThreshold: z.string(),
});

export const settingsFormSchema = z.object({
  downloader: downloaderSchema,
  filter_: filterSchema,
  notification: notificationSchema,
  priority: prioritySchema,
  tmdb: tmdbSchema,
  emby: embySchema,
  ai: aiSchema,
});

export type DownloaderFormData = z.infer<typeof downloaderSchema>;
export type FilterFormData = z.infer<typeof filterSchema>;
export type TelegramConfigFormData = z.infer<typeof telegramConfigSchema>;
export type NotificationFormData = z.infer<typeof notificationSchema>;
export type GroupFormData = z.infer<typeof groupSchema>;
export type PriorityFormData = z.infer<typeof prioritySchema>;
export type TmdbFormData = z.infer<typeof tmdbSchema>;
export type EmbyFormData = z.infer<typeof embySchema>;
export type AiFormData = z.infer<typeof aiSchema>;
export type SettingsFormData = z.infer<typeof settingsFormSchema>;
