import { useEffect, useRef } from "react";
import { useForm } from "@tanstack/react-form";
import { useQuery, useMutation, useQueryClient } from "@tanstack/react-query";
import { getApiSettings, putApiSettings } from "@/client/sdk.gen";
import {
  settingsFormSchema,
  mergeWithDefaultPatterns,
  fromUserPreference,
  toUserPreference,
  type SettingsFormData,
  type ApiUserPreference,
} from "../schema";

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
      libraryId: "",
      libraryName: "",
    },
    ai: {
      apiKey: "",
      model: "xiaomi/mimo-v2-flash:free",
      confidenceThreshold: "0.8",
    },
  };
}

export function useSettingsForm() {
  const queryClient = useQueryClient();

  const { data: serverData, isLoading, error } = useQuery({
    queryKey: ["settings"],
    queryFn: async () => {
      const { data } = await getApiSettings({ throwOnError: true });
      return data;
    },
  });

  const defaultValues = serverData
    ? fromUserPreference(serverData as ApiUserPreference)
    : getDefaultFormData();

  const form = useForm({
    defaultValues,
    validators: {
      onChange: settingsFormSchema,
    },
  });

  const hasReset = useRef(false);
  useEffect(() => {
    if (serverData && !hasReset.current) {
      hasReset.current = true;
      form.reset(fromUserPreference(serverData as ApiUserPreference));
    }
  }, [serverData, form]);

  const mutation = useMutation({
    mutationFn: async (values: SettingsFormData) => {
      const body = toUserPreference(values);
      // eslint-disable-next-line @typescript-eslint/no-explicit-any
      const { data } = await putApiSettings({ body: body as any, throwOnError: true });
      return data;
    },
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ["settings"] });
    },
  });

  return { form, mutation, isLoading, error };
}

export type SettingsFormInstance = ReturnType<typeof useSettingsForm>["form"];

export function getErrorMessage(error: unknown): string | undefined {
  if (!error) return undefined;
  if (typeof error === "string") return error;
  if (typeof error === "object" && "message" in error) {
    return (error as { message: string }).message;
  }
  return String(error);
}
