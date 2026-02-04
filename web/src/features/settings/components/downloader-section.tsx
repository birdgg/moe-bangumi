import * as React from "react";
import { Input } from "@/components/ui/input";
import {
  IconEye,
  IconEyeOff,
  IconFolder,
} from "@tabler/icons-react";
import { FormField } from "./shared";
import { getErrorMessage, type SettingsFormInstance } from "../hooks";

export interface DownloaderSectionProps {
  form: SettingsFormInstance;
}

export function DownloaderSection({ form }: DownloaderSectionProps) {
  const [showPassword, setShowPassword] = React.useState(false);

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
      </div>
    </section>
  );
}
