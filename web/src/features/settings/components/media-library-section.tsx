import * as React from "react";
import { useMutation, useQueryClient } from "@tanstack/react-query";
import {
  postApiMediaLibraries,
  postApiMediaImport,
} from "@/client/sdk.gen";
import { getApiTrackingBangumisQueryKey } from "@/client/@tanstack/react-query.gen";
import type { MediaLibrary } from "@/client/types.gen";
import { Button } from "@/components/ui/button";
import { Input } from "@/components/ui/input";
import {
  Select,
  SelectContent,
  SelectItem,
  SelectTrigger,
  SelectValue,
} from "@/components/ui/select";
import { Spinner } from "@/components/ui/spinner";
import {
  IconEye,
  IconEyeOff,
  IconKey,
  IconServer,
  IconLink,
  IconDownload,
} from "@tabler/icons-react";
import { FormField } from "./shared";
import { getErrorMessage, type SettingsFormInstance } from "../hooks";

export interface MediaLibrarySectionProps {
  form: SettingsFormInstance;
}

function collectionTypeLabel(type: string | undefined): string {
  const labels: Record<string, string> = {
    tvshows: "TV",
    movies: "Movie",
    music: "Music",
    books: "Book",
    photos: "Photo",
    homevideos: "Home Video",
    boxsets: "Box Set",
  };
  if (!type) return "";
  return labels[type] ?? type;
}

export function MediaLibrarySection({ form }: MediaLibrarySectionProps) {
  const queryClient = useQueryClient();
  const [showEmbyApiKey, setShowEmbyApiKey] = React.useState(false);
  const [libraries, setLibraries] = React.useState<MediaLibrary[]>([]);

  const librariesMutation = useMutation({
    mutationFn: async () => {
      const values = form.state.values.emby;
      const { data } = await postApiMediaLibraries({
        body: { url: values.url, apiKey: values.apiKey },
        throwOnError: true,
      });
      return data;
    },
    onSuccess: (data) => {
      setLibraries(data ?? []);
    },
  });

  const importMutation = useMutation({
    mutationFn: async () => {
      const values = form.state.values.emby;
      const { data } = await postApiMediaImport({
        body: {
          url: values.url,
          apiKey: values.apiKey,
          libraryId: values.libraryId,
        },
        throwOnError: true,
      });
      return data;
    },
    onSuccess: () => {
      queryClient.invalidateQueries({
        queryKey: getApiTrackingBangumisQueryKey(),
      });
    },
  });

  const handleSelectOpen = (open: boolean) => {
    if (!open) return;
    const { url, apiKey } = form.state.values.emby;
    if (url && apiKey) {
      librariesMutation.mutate();
    }
  };

  return (
    <section className="space-y-5">
      <div className="space-y-4">
        <div className="rounded-lg border border-border/50 bg-muted/20 p-4">
          <div className="flex items-center gap-2 pb-3">
            <IconServer className="size-4 text-emerald-500" />
            <span className="text-sm font-medium">Emby</span>
          </div>
          <p className="pb-3 text-xs text-muted-foreground">
            连接 Emby
            媒体服务器，可从媒体库导入番剧信息。填写服务器地址和 API Key
            后即可使用。
          </p>

          <div className="space-y-4">
            <form.Field name="emby.url">
              {(field) => {
                const error = getErrorMessage(field.state.meta.errors[0]);
                return (
                  <FormField label="服务器地址">
                    <div className="relative">
                      <div className="pointer-events-none absolute left-2.5 top-1/2 -translate-y-1/2 text-muted-foreground">
                        <IconLink className="size-4" />
                      </div>
                      <Input
                        type="url"
                        placeholder="http://localhost:8096"
                        value={field.state.value ?? ""}
                        onChange={(e) => field.handleChange(e.target.value)}
                        onBlur={field.handleBlur}
                        className="pl-9"
                      />
                    </div>
                    {error && (
                      <p className="text-xs text-destructive mt-1">{error}</p>
                    )}
                  </FormField>
                );
              }}
            </form.Field>

            <form.Field name="emby.apiKey">
              {(field) => {
                const error = getErrorMessage(field.state.meta.errors[0]);
                return (
                  <FormField label="API Key">
                    <div className="relative">
                      <div className="pointer-events-none absolute left-2.5 top-1/2 -translate-y-1/2 text-muted-foreground">
                        <IconKey className="size-4" />
                      </div>
                      <Input
                        type={showEmbyApiKey ? "text" : "password"}
                        placeholder="输入你的 Emby API Key"
                        value={field.state.value ?? ""}
                        onChange={(e) => field.handleChange(e.target.value)}
                        onBlur={field.handleBlur}
                        className="pl-9 pr-10 font-mono text-sm"
                      />
                      <button
                        type="button"
                        onClick={() => setShowEmbyApiKey(!showEmbyApiKey)}
                        className="absolute right-2 top-1/2 -translate-y-1/2 p-1 text-muted-foreground transition-colors hover:text-foreground cursor-pointer"
                      >
                        {showEmbyApiKey ? (
                          <IconEyeOff className="size-4" />
                        ) : (
                          <IconEye className="size-4" />
                        )}
                      </button>
                    </div>
                    {error && (
                      <p className="text-xs text-destructive mt-1">{error}</p>
                    )}
                  </FormField>
                );
              }}
            </form.Field>
          </div>

          {/* Library Selection */}
          <div className="mt-4 pt-4 border-t border-border/30 space-y-3">
            <form.Field name="emby.libraryId">
              {(field) => (
                <FormField label="选择媒体库">
                  <Select
                    value={field.state.value || null}
                    onValueChange={(val) => {
                      if (!val) return;
                      field.handleChange(val);
                      const lib = libraries.find((l) => l.libraryId === val);
                      form.setFieldValue("emby.libraryName", lib?.libraryName ?? "");
                    }}
                    onOpenChange={handleSelectOpen}
                  >
                    <SelectTrigger className="w-full">
                      {librariesMutation.isPending ? (
                        <div className="flex items-center gap-2 text-muted-foreground">
                          <Spinner className="size-3" />
                          <span>获取中...</span>
                        </div>
                      ) : (
                        <SelectValue placeholder="选择要导入的媒体库">
                          {field.state.value
                            ? (libraries.find(
                                (lib) =>
                                  lib.libraryId === field.state.value,
                              )?.libraryName ??
                              form.state.values.emby.libraryName) ||
                              undefined
                            : undefined}
                        </SelectValue>
                      )}
                    </SelectTrigger>
                    <SelectContent>
                      {libraries.map((lib) => (
                        <SelectItem key={lib.libraryId} value={lib.libraryId}>
                          {lib.libraryName}
                          {lib.collectionType && (
                            <span className="text-muted-foreground ml-1">
                              ({collectionTypeLabel(lib.collectionType)})
                            </span>
                          )}
                        </SelectItem>
                      ))}
                    </SelectContent>
                  </Select>
                  {librariesMutation.isError && (
                    <p className="text-xs text-destructive mt-1">
                      获取媒体库失败，请检查服务器地址和 API Key 是否正确
                    </p>
                  )}
                </FormField>
              )}
            </form.Field>

            <form.Subscribe selector={(state) => state.values.emby.libraryId}>
              {(libraryId) =>
                libraryId ? (
                  <div className="flex items-center gap-2">
                    <Button
                      type="button"
                      variant="outline"
                      size="sm"
                      disabled={importMutation.isPending}
                      onClick={() => importMutation.mutate()}
                    >
                      {importMutation.isPending ? (
                        <>
                          <Spinner className="size-4" />
                          <span>导入中...</span>
                        </>
                      ) : (
                        <>
                          <IconDownload className="size-4" />
                          <span>导入</span>
                        </>
                      )}
                    </Button>
                    {importMutation.data && (
                      <span className="text-xs text-emerald-600">
                        成功导入 {importMutation.data.imported} 部
                        {importMutation.data.failed > 0 && (
                          <span className="text-destructive">
                            ，{importMutation.data.failed} 部失败
                          </span>
                        )}
                      </span>
                    )}
                    {importMutation.isError && (
                      <span className="text-xs text-destructive">
                        导入失败
                      </span>
                    )}
                  </div>
                ) : null
              }
            </form.Subscribe>
          </div>

          <div className="mt-4 pt-4 border-t border-border/30">
            <p className="text-xs text-muted-foreground">
              如何获取 API Key？
            </p>
            <ul className="mt-1 text-xs text-muted-foreground list-disc list-inside space-y-0.5">
              <li>在 Emby 服务器中进入「设置」→「API 密钥」</li>
              <li>点击「新建 API 密钥」并输入应用名称</li>
            </ul>
          </div>
        </div>
      </div>
    </section>
  );
}
