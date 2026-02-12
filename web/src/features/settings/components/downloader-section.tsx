import * as React from "react";
import { useMutation } from "@tanstack/react-query";
import { postApiDownloaderTest } from "@/client/sdk.gen";
import { Button } from "@/components/ui/button";
import { Input } from "@/components/ui/input";
import { Spinner } from "@/components/ui/spinner";
import {
  IconEye,
  IconEyeOff,
  IconFolder,
  IconPlugConnected,
} from "@tabler/icons-react";
import { FormField } from "./shared";
import { getErrorMessage, type SettingsFormInstance } from "../hooks";

export interface DownloaderSectionProps {
  form: SettingsFormInstance;
}

export function DownloaderSection({ form }: DownloaderSectionProps) {
  const [showPassword, setShowPassword] = React.useState(false);

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
