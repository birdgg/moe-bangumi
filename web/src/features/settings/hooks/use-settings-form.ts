import { useForm } from "@tanstack/react-form";
import { settingsFormSchema, mergeWithDefaultPatterns, type SettingsFormData } from "../schema";

function getDefaultFormData(): SettingsFormData {
  return {
    downloader: {
      url: "http://localhost:8080",
      username: "",
      password: "",
      savePath: "/Media/Bangumi",
    },
    filter_: {
      globalRssFilters: [],
    },
    notification: {
      telegram: {
        botToken: "",
        chatId: "",
      },
    },
    priority: {
      groups: [],
      languages: mergeWithDefaultPatterns([]),
    },
    tmdb: {
      apiKey: "",
    },
    emby: {
      url: "",
      apiKey: "",
    },
    ai: {
      apiKey: "",
      model: "xiaomi/mimo-v2-flash:free",
      confidenceThreshold: "0.8",
    },
  };
}

export function useSettingsForm() {
  const form = useForm({
    defaultValues: getDefaultFormData(),
    validators: {
      onChange: settingsFormSchema,
    },
  });

  return form;
}

export type SettingsFormInstance = ReturnType<typeof useSettingsForm>;

export function getErrorMessage(error: unknown): string | undefined {
  if (!error) return undefined;
  if (typeof error === "string") return error;
  if (typeof error === "object" && "message" in error) {
    return (error as { message: string }).message;
  }
  return String(error);
}
