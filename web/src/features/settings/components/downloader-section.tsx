import * as React from "react";
import { useMutation, useQueryClient } from "@tanstack/react-query";
import { postApiDownloaderTest } from "@/client/sdk.gen";
import { getApiTrackingBangumisQueryKey } from "@/client/@tanstack/react-query.gen";
import { client } from "@/client/client.gen";
import { toast } from "sonner";
import { Button } from "@/components/ui/button";
import { Input } from "@/components/ui/input";
import { Spinner } from "@/components/ui/spinner";
import {
  IconEye,
  IconEyeOff,
  IconFolder,
  IconPlugConnected,
  IconSearch,
} from "@tabler/icons-react";
import { FormField } from "./shared";
import { getErrorMessage, type SettingsFormInstance } from "../hooks";

export interface DownloaderSectionProps {
  form: SettingsFormInstance;
}

// TODO: replace with generated `postApiImportScan` from sdk.gen after running `just gen-api`
interface ImportScanResponse {
  imported: { bangumiId: number; title: string; posterUrl?: string; maxEpisode: number }[];
  skipped: { folderName: string; reason: string }[];
}

export function DownloaderSection({ form }: DownloaderSectionProps) {
  const [showPassword, setShowPassword] = React.useState(false);
  const queryClient = useQueryClient();

  const scanMutation = useMutation({
    mutationFn: async () => {
      const { data } = await client.post<ImportScanResponse, unknown, true>({
        url: "/api/import/scan",
        throwOnError: true,
      });
      return data;
    },
    onSuccess: (data) => {
      const importedCount = data.imported.length;
      const skippedCount = data.skipped.length;

      if (importedCount > 0) {
        queryClient.invalidateQueries({ queryKey: getApiTrackingBangumisQueryKey() });
        toast.success(`导入了 ${importedCount} 部番剧`, {
          description:
            skippedCount > 0 ? `跳过了 ${skippedCount} 部` : undefined,
        });
      } else if (skippedCount > 0) {
        toast.info(`跳过了 ${skippedCount} 部番剧（均已存在或无法识别）`);
      } else {
        toast.info("未发现任何番剧目录");
      }
    },
    onError: () => {
      toast.error("扫描失败");
    },
  });

  const testMutation = useMutation({
    mutationFn: async () => {
      const values = form.state.values.downloader;
      const { data } = await postApiDownloaderTest({
        body: {
          url: values.url,
          username: values.username,
          password: values.password,
        },
        throwOnError: true,
      });
      return data;
    },
  });

  const testResult = testMutation.data;
  const testError = testMutation.isError;

  return (
    <section className="space-y-5">
      <div className="space-y-4">
        <form.Field name="downloader.url">
          {(field) => {
            const error = getErrorMessage(field.state.meta.errors[0]);
            return (
              <FormField label="服务器地址">
                <Input
                  type="url"
                  placeholder="http://localhost:8080"
                  value={field.state.value}
                  onChange={(e) => field.handleChange(e.target.value)}
                  onBlur={field.handleBlur}
                />
                {error && (
                  <p className="text-xs text-destructive mt-1">{error}</p>
                )}
              </FormField>
            );
          }}
        </form.Field>

        <div className="grid gap-4 sm:grid-cols-2">
          <form.Field name="downloader.username">
            {(field) => {
              const error = getErrorMessage(field.state.meta.errors[0]);
              return (
                <FormField label="用户名">
                  <Input
                    type="text"
                    placeholder="admin"
                    value={field.state.value}
                    onChange={(e) => field.handleChange(e.target.value)}
                    onBlur={field.handleBlur}
                  />
                  {error && (
                    <p className="text-xs text-destructive mt-1">{error}</p>
                  )}
                </FormField>
              );
            }}
          </form.Field>

          <form.Field name="downloader.password">
            {(field) => {
              const error = getErrorMessage(field.state.meta.errors[0]);
              return (
                <FormField label="密码">
                  <div className="relative">
                    <Input
                      type={showPassword ? "text" : "password"}
                      placeholder="********"
                      value={field.state.value}
                      onChange={(e) => field.handleChange(e.target.value)}
                      onBlur={field.handleBlur}
                      className="pr-10"
                    />
                    <button
                      type="button"
                      onClick={() => setShowPassword(!showPassword)}
                      className="absolute right-2 top-1/2 -translate-y-1/2 p-1 text-muted-foreground transition-colors hover:text-foreground cursor-pointer"
                    >
                      {showPassword ? (
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

        <form.Field name="downloader.savePath">
          {(field) => {
            const error = getErrorMessage(field.state.meta.errors[0]);
            return (
              <FormField label="默认保存路径">
                <div className="relative">
                  <div className="pointer-events-none absolute left-2.5 top-1/2 -translate-y-1/2 text-muted-foreground">
                    <IconFolder className="size-4" />
                  </div>
                  <Input
                    type="text"
                    placeholder="/Media/Bangumi"
                    value={field.state.value}
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

        <form.Subscribe selector={(s) => s.values.downloader}>
          {(downloader) => (
            <div className="flex items-center gap-2">
              <Button
                type="button"
                variant="outline"
                size="sm"
                disabled={
                  testMutation.isPending ||
                  !downloader.url ||
                  !downloader.username ||
                  !downloader.password
                }
                onClick={() => testMutation.mutate()}
              >
                {testMutation.isPending ? (
                  <>
                    <Spinner className="size-4" />
                    <span>测试中...</span>
                  </>
                ) : (
                  <>
                    <IconPlugConnected className="size-4" />
                    <span>测试连接</span>
                  </>
                )}
              </Button>
              <Button
                type="button"
                variant="outline"
                size="sm"
                disabled={scanMutation.isPending}
                onClick={() => scanMutation.mutate()}
              >
                {scanMutation.isPending ? (
                  <>
                    <Spinner className="size-4" />
                    <span>扫描中...</span>
                  </>
                ) : (
                  <>
                    <IconSearch className="size-4" />
                    <span>扫描并导入</span>
                  </>
                )}
              </Button>
              {testResult?.success && (
                <span className="text-xs text-emerald-600">
                  {testResult.version
                    ? `连接成功 (${testResult.version})`
                    : "连接成功"}
                </span>
              )}
              {testResult && !testResult.success && (
                <span className="text-xs text-destructive">
                  {testResult.message}
                </span>
              )}
              {testError && (
                <span className="text-xs text-destructive">连接失败</span>
              )}
            </div>
          )}
        </form.Subscribe>
      </div>
    </section>
  );
}
