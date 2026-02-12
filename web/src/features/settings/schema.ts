import { z } from "zod";

export const subtitleLanguages = ["chs", "cht", "jpn", "eng"] as const;
export type SubtitleLanguage = (typeof subtitleLanguages)[number];

export type SubtitlePattern = SubtitleLanguage[];

export const defaultSubtitlePatterns: SubtitlePattern[] = [
  ["chs", "jpn"],
  ["cht", "jpn"],
  ["chs"],
  ["cht"],
];

export function subtitlePatternLabel(pattern: SubtitlePattern): string {
  const sorted = [...pattern].sort();
  const key = sorted.join("+");
  const labels: Record<string, string> = {
    "chs": "简",
    "cht": "繁",
    "chs+jpn": "简日",
    "cht+jpn": "繁日",
    "jpn": "日",
    "eng": "英",
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
  libraryId: z.string(),
  libraryName: z.string(),
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

/** Shape of the UserPreference payload sent to/from the API.
 *  Defined locally since the auto-generated types may lag behind the backend. */
export interface ApiUserPreference {
  downloader?: { url: string; username: string; password: string; savePath: string };
  filter?: { globalRssFilter: string[] };
  washing?: { groupPriority: GroupFormData[]; subtitlePriority: SubtitlePattern[] };
  notification?: { botToken: string; chatId: string };
  tmdb?: { apiKey: string; language?: string };
  emby?: { url: string; apiKey: string; libraryId: string; libraryName: string };
}

/** Convert API UserPreference to form data. */
export function fromUserPreference(pref: ApiUserPreference): SettingsFormData {
  return {
    downloader: {
      url: pref.downloader?.url ?? "",
      username: pref.downloader?.username ?? "",
      password: pref.downloader?.password ?? "",
      savePath: pref.downloader?.savePath ?? "",
    },
    filter_: {
      globalRssFilters: pref.filter?.globalRssFilter ?? [],
    },
    notification: {
      telegram: {
        botToken: pref.notification?.botToken ?? "",
        chatId: pref.notification?.chatId ?? "",
      },
    },
    priority: {
      groups: pref.washing?.groupPriority ?? [],
      languages: mergeWithDefaultPatterns(
        (pref.washing?.subtitlePriority ?? []) as SubtitlePattern[]
      ),
    },
    tmdb: {
      apiKey: pref.tmdb?.apiKey ?? "",
    },
    emby: {
      url: pref.emby?.url ?? "",
      apiKey: pref.emby?.apiKey ?? "",
      libraryId: pref.emby?.libraryId ?? "",
      libraryName: pref.emby?.libraryName ?? "",
    },
    ai: {
      apiKey: "",
      model: "xiaomi/mimo-v2-flash:free",
      confidenceThreshold: "0.8",
    },
  };
}

/** Convert form data to API UserPreference for saving. */
export function toUserPreference(data: SettingsFormData): ApiUserPreference {
  return {
    downloader: {
      url: data.downloader.url,
      username: data.downloader.username,
      password: data.downloader.password,
      savePath: data.downloader.savePath,
    },
    filter: {
      globalRssFilter: data.filter_.globalRssFilters,
    },
    washing: {
      groupPriority: data.priority.groups,
      subtitlePriority: data.priority.languages,
    },
    notification: {
      botToken: data.notification.telegram.botToken,
      chatId: data.notification.telegram.chatId,
    },
    tmdb: {
      apiKey: data.tmdb.apiKey,
      language: "zh-CN",
    },
    emby: {
      url: data.emby.url,
      apiKey: data.emby.apiKey,
      libraryId: data.emby.libraryId,
      libraryName: data.emby.libraryName,
    },
  };
}
